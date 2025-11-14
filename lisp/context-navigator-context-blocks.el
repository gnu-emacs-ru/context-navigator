;;; context-navigator-context-blocks.el --- Org context blocks import/export -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; §22 Context Blocks — insert current context as a mixed-format block and
;; apply (add/replace) items from a block at point.
;;
;; Grammar (no params):
;; #+begin_context
;;  /abs/or/rel/path.ext
;;  file:/abs/or/rel/path.ext
;;  sel:/abs/or/rel/path.ext:BEG-END
;;  buf:<BUFFER-NAME>
;;  buf:<BUFFER-NAME>:/abs/or/rel/path.ext
;; #+end_context
;;
;; Relative paths are resolved against the current project root (§06).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-project)
(require 'context-navigator-log)
(require 'context-navigator-add-paths)  ;; filters/limits/TRAMP policies (§13)
(require 'context-navigator-i18n)       ;; i18n labels for messages (§16)

;; Optional org-element usage (fallback to regex when absent)
(defvar context-navigator-context-blocks--use-org
  (ignore-errors (require 'org-element nil t)))

(defun cn-ctxblk--project-root ()
  "Return current project root or nil."
  (condition-case _err
      (let* ((st (and (fboundp 'context-navigator--state-get)
                      (ignore-errors (context-navigator--state-get))))
             (root (and st (ignore-errors (context-navigator-state-last-project-root st)))))
        (or root
            (and (fboundp 'context-navigator-project-root)
                 (ignore-errors (context-navigator-project-root (current-buffer))))))
    (error nil)))

(defun cn-ctxblk--bounds ()
  "Return cons of (BEG . END) for the body of a context block at point, or nil."
  (save-excursion
    (cond
     (context-navigator-context-blocks--use-org
      (condition-case _err
          (let* ((elem (org-element-context))
                 (blk  (cl-loop for e = elem then (org-element-property :parent e)
                                while e
                                when (eq (car e) 'special-block)
                                return e))
                 (type (and blk (org-element-property :type blk))))
            (when (and (stringp type) (string-match-p "\\`context\\'" (downcase type)))
              (let ((b (org-element-property :contents-begin blk))
                    (e (org-element-property :contents-end blk)))
                (and (integerp b) (integerp e) (cons b e)))))
        (error nil)))
     (t
      ;; Regex fallback: search nearest begin/end around point (case-insensitive)
      (let (b e)
        (save-excursion
          (when (re-search-backward "^[ \t]*#\\+begin_context\\b" nil t)
            (setq b (match-end 0))
            (when (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
              (setq e (match-beginning 0)))))
        (and (integerp b) (integerp e) (cons b e)))))))

(defun cn-ctxblk--block-range ()
  "Return cons of (BEGIN . END) covering the entire context block at point, or nil."
  (save-excursion
    (cond
     (context-navigator-context-blocks--use-org
      (condition-case _err
          (let* ((elem (org-element-context))
                 (blk  (cl-loop for e = elem then (org-element-property :parent e)
                                while e
                                when (eq (car e) 'special-block)
                                return e))
                 (type (and blk (org-element-property :type blk))))
            (when (and (stringp type) (string-match-p "\\`context\\'" (downcase type)))
              (let ((b (org-element-property :begin blk))
                    (e (org-element-property :end blk)))
                (and (integerp b) (integerp e) (cons b e)))))
        (error nil)))
     (t
      ;; Regex fallback: search nearest begin/end around point (case-insensitive)
      (let (b e)
        (save-excursion
          (when (re-search-backward "^[ \t]*#\\+begin_context\\b" nil t)
            (setq b (match-beginning 0))
            (when (re-search-forward "^[ \t]*#\\+end_context\\b" nil t)
              (setq e (line-end-position))
              (when (and (integerp e)
                         (< e (point-max))
                         (eq (char-after e) ?\n))
                (setq e (1+ e)))))))
      (and (integerp b) (integerp e) (cons b e))))))

(defun cn-ctxblk--expand-path (path &optional root)
  "Resolve PATH to an absolute normalized path, using ROOT for relative names."
  (let* ((r (or root (cn-ctxblk--project-root)))
         (p (string-trim (or path ""))))
    (cond
     ((string-empty-p p) nil)
     ((file-name-absolute-p p) (expand-file-name p))
     ((string-prefix-p "~" p)  (expand-file-name p))
     ((stringp r) (expand-file-name p (file-name-as-directory (expand-file-name r))))
     (t (expand-file-name p)))))

(defun cn-ctxblk--live-buffer-by-name (name)
  "Return live buffer by NAME or nil."
  (when (and (stringp name) (not (string-empty-p name)))
    (get-buffer name)))

(defun cn-ctxblk--parse-sel (line root)
  "Parse selection LINE with prefix sel:, return item or nil."
  (when (string-match "\\`sel:\\(.*\\):\\([0-9]+\\)-\\([0-9]+\\)\\'" line)
    (let* ((p0 (match-string 1 line))
           (b  (string-to-number (match-string 2 line)))
           (e  (string-to-number (match-string 3 line)))
           (abs (cn-ctxblk--expand-path p0 root)))
      (when (and abs (file-exists-p abs) (file-regular-p abs))
        (context-navigator-item-create
         :type 'selection
         :name (format "%s:%d-%d" (file-name-nondirectory abs) b e)
         :path abs
         :beg (min b e)
         :end (max b e)
         :enabled t)))))

(defun cn-ctxblk--parse-buf (line root)
  "Parse buffer LINE with prefix buf:, return item (buffer/file) or nil.
If NAME is not live and PATH is present, downgrade to a file item."
  (when (string-prefix-p "buf:" line)
    (let* ((rest (substring line 4))
           (idx (cl-position ?: rest :from-end t))
           (name (string-trim (if idx (substring rest 0 idx) rest)))
           (path (and idx (string-trim (substring rest (1+ idx)))))
           (buf  (cn-ctxblk--live-buffer-by-name name))
           (abs  (and path (cn-ctxblk--expand-path path root))))
      (cond
       ((buffer-live-p buf)
        (context-navigator-item-create
         :type 'buffer
         :name (buffer-name buf)
         :path (and abs (file-exists-p abs) abs)
         :buffer buf
         :enabled t))
       ((and abs (file-exists-p abs) (file-regular-p abs))
        (context-navigator-item-create
         :type 'file
         :name (file-name-nondirectory abs)
         :path abs
         :enabled t))
       (t nil)))))

(defun cn-ctxblk--parse-file (line root)
  "Parse file LINE which may start with file: or be a plain path."
  (let* ((p0 (if (string-prefix-p "file:" line)
                 (substring line 5)
               line))
         (abs (cn-ctxblk--expand-path p0 root)))
    (when (and abs (file-exists-p abs) (file-regular-p abs) (not (file-symlink-p abs)))
      (context-navigator-item-create
       :type 'file
       :name (file-name-nondirectory abs)
       :path abs
       :enabled t))))

(defun cn-ctxblk--parse-one (line root)
  "Parse one LINE (trimmed) into an item or nil."
  (let ((s (string-trim line)))
    (cond
     ((or (string-empty-p s) (string-prefix-p "#" s)) nil)
     ((string-prefix-p "sel:" s) (cn-ctxblk--parse-sel s root))
     ((string-prefix-p "buf:" s) (cn-ctxblk--parse-buf s root))
     ((or (string-prefix-p "file:" s)
          (file-name-absolute-p s) (string-prefix-p "~" s)
          t)
      (cn-ctxblk--parse-file s root))
     (t nil))))

(defun cn-ctxblk--parse-lines (text)
  "Parse TEXT lines from a context block body into items and stats plist."
  (let* ((root (cn-ctxblk--project-root))
         (lines (split-string (or text "") "\n" t))
         (items '())
         (skipped 0))
    (dolist (ln lines)
      (let ((it (ignore-errors (cn-ctxblk--parse-one ln root))))
        (if (context-navigator-item-p it)
            (push it items)
          (setq skipped (1+ skipped)))))
    (let* ((rev (nreverse items))
           (uniq (context-navigator-model-uniq rev))
           (files (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'file)) uniq))
           (bufs  (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'buffer)) uniq))
           (sels  (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'selection)) uniq)))
      (list :items uniq
            :files files :buffers bufs :selections sels
            :skipped skipped
            :total (length lines)))))

(defun cn-ctxblk--filter-items (items)
  "Filter ITEMS according to filesystem policy §13.2.
Returns plist:
 :items list<context-navigator-item> (filtered)
 :skipped-too-big N
 :skipped-nonregular N
 :remote N"
  (let ((keep '())
        (too-big 0)
        (nonreg 0)
        (remote 0)
        (limit (or (and (boundp 'context-navigator-max-file-size)
                        context-navigator-max-file-size)
                   most-positive-fixnum)))
    (dolist (it items)
      (pcase (context-navigator-item-type it)
        ('file
         (let* ((p (context-navigator-item-path it)))
           (cond
            ((or (not (stringp p))
                 (not (file-exists-p p))
                 (not (file-regular-p p))
                 (context-navigator-path-add--symlink-p p))
             (setq nonreg (1+ nonreg)))
            (t
             (when (file-remote-p p) (setq remote (1+ remote)))
             (let ((sz (context-navigator-path-add--file-size p)))
               (if (and sz (> sz limit))
                   (setq too-big (1+ too-big))
                 (push it keep)))))))
        ('selection
         (let* ((p (context-navigator-item-path it)))
           (cond
            ((or (not (stringp p))
                 (not (file-exists-p p))
                 (not (file-regular-p p))
                 (context-navigator-path-add--symlink-p p))
             (setq nonreg (1+ nonreg)))
            (t
             (when (file-remote-p p) (setq remote (1+ remote)))
             (push it keep)))))
        ('buffer
         (let* ((p (context-navigator-item-path it)))
           (when (and (stringp p) (file-remote-p p)) (setq remote (1+ remote))))
         (push it keep))
        (_ (push it keep))))
    (list :items (nreverse keep)
          :skipped-too-big too-big
          :skipped-nonregular nonreg
          :remote remote)))

(defun cn-ctxblk--format-item (it)
  "Format an item IT as a line for a context block."
  (pcase (context-navigator-item-type it)
    ('file
     (format " %s" (expand-file-name (context-navigator-item-path it))))
    ('selection
     (let* ((p (expand-file-name (context-navigator-item-path it)))
            (b (context-navigator-item-beg it))
            (e (context-navigator-item-end it)))
       (format " sel:%s:%d-%d" p b e)))
    ('buffer
     (let* ((nm (context-navigator-item-name it))
            (p  (context-navigator-item-path it)))
       (if (and (stringp p) (not (string-empty-p p)))
           (format " buf:%s:%s" nm (expand-file-name p))
         (format " buf:%s" nm))))
    (_ nil)))

(defun cn-ctxblk--emit-block (items)
  "Return a string with a #+begin_context block representing ITEMS.
Files first (sorted by relpath), then selections, then buffers."
  (let* ((root (cn-ctxblk--project-root))
         (files (cl-remove-if-not (lambda (x) (eq (context-navigator-item-type x) 'file)) items))
         (sels  (cl-remove-if-not (lambda (x) (eq (context-navigator-item-type x) 'selection)) items))
         (bufs  (cl-remove-if-not (lambda (x) (eq (context-navigator-item-type x) 'buffer)) items))
         (relkey (lambda (p)
                   (condition-case _err
                       (if (stringp root)
                           (file-relative-name (expand-file-name p)
                                               (file-name-as-directory (expand-file-name root)))
                         (expand-file-name p))
                     (error (expand-file-name p)))))
         (files* (sort (copy-sequence files)
                       (lambda (a b)
                         (string-lessp (funcall relkey (context-navigator-item-path a))
                                       (funcall relkey (context-navigator-item-path b))))))
         (all (append files* sels bufs))
         (body (mapcar #'cn-ctxblk--format-item all)))
    (concat "#+begin_context\n"
            (mapconcat #'identity (cl-remove-if #'null body) "\n")
            (when body "\n")
            "#+end_context\n")))

(defun cn-ctxblk--enabled-items (items)
  "Filter to enabled ITEMS."
  (cl-remove-if-not (lambda (x) (context-navigator-item-enabled x)) (or items '())))

;;;###autoload
(defun context-navigator-context-block-insert ()
  "Insert at point a context block representing current group's enabled items."
  (interactive)
  (let* ((st (and (fboundp 'context-navigator--state-get)
                  (ignore-errors (context-navigator--state-get))))
         (items (and st (ignore-errors (context-navigator-state-items st))))
         (enabled (cn-ctxblk--enabled-items items))
         (txt (cn-ctxblk--emit-block enabled)))
    (if-let ((range (cn-ctxblk--block-range)))
        (let ((start (car range))
              (end (cdr range)))
          (delete-region start end)
          (goto-char start)
          (insert txt))
      (insert txt))
    (when (fboundp 'context-navigator-ui-info)
      (context-navigator-ui-info :ctxblk-insert))))

(defun cn-ctxblk--block-text-at-point ()
  "Return text inside the context block at point, or nil."
  (when-let ((bnds (cn-ctxblk--bounds)))
    (buffer-substring-no-properties (car bnds) (cdr bnds)))

  )

(defun cn-ctxblk--apply-items (items &optional replace)
  "Apply ITEMS to model. When REPLACE is non-nil, replace current items."
  (let* ((st (and (fboundp 'context-navigator--state-get)
                  (ignore-errors (context-navigator--state-get))))
         (old (and st (ignore-errors (context-navigator-state-items st)))))
    (cond
     (replace
      (when (fboundp 'context-navigator-snapshot-push)
        (ignore-errors (context-navigator-snapshot-push)))
      (ignore-errors (context-navigator-set-items (or items '()))))
     (t
      (let* ((merged (context-navigator-model-uniq (append (or old '()) (or items '())))))
        (ignore-errors (context-navigator-set-items merged)))))))

(defun cn-ctxblk--apply-from-block (mode)
  "Internal: MODE is one of 'add or 'replace (see §22.4)."
  (let* ((btxt (cn-ctxblk--block-text-at-point)))
    (if (not btxt)
        (when (fboundp 'context-navigator-ui-info)
          (context-navigator-ui-info :ctxblk-not-in-block))
      (let* ((res0 (cn-ctxblk--parse-lines btxt))
             (parsed-items (plist-get res0 :items)))
        (if (not (listp parsed-items))
            (when (fboundp 'context-navigator-ui-info)
              (context-navigator-ui-info :ctxblk-parse-error))
          ;; Filters/limits/remote counters (CN-ADD-FS-POLICY)
          (let* ((flt (cn-ctxblk--filter-items parsed-items))
                 (items (plist-get flt :items))
                 ;; Count types after filtering for clean summary
                 (files (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'file)) items))
                 (sels  (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'selection)) items))
                 (bufs  (cl-count-if (lambda (x) (eq (context-navigator-item-type x) 'buffer)) items))
                 (skipped-lines (or (plist-get res0 :skipped) 0))
                 (skipped-too-big (or (plist-get flt :skipped-too-big) 0))
                 (skipped-nonregular (or (plist-get flt :skipped-nonregular) 0))
                 (remote (or (plist-get flt :remote) 0))
                 (skipped-total (+ skipped-lines skipped-too-big skipped-nonregular))
                 (limit (or (bound-and-true-p context-navigator-path-add-limit) 70)))
            ;; Path-add-limit for files (CN-ADD-RESULT)
            (when (> files limit)
              (when (fboundp 'context-navigator-ui-warn)
                (context-navigator-ui-warn :too-many files limit))
              (when (fboundp 'context-navigator-ui-info)
                (context-navigator-ui-info :aborted))
              (cl-return-from cn-ctxblk--apply-from-block nil))
            ;; Remote/TRAMP confirm (CN-ADD-REMOTE-CONFIRM)
            (when (and (> remote 0)
                       (fboundp 'context-navigator-ui-ask)
                       (not (context-navigator-ui-ask :remote-warning remote)))
              (when (fboundp 'context-navigator-ui-info)
                (context-navigator-ui-info :aborted))
              (cl-return-from cn-ctxblk--apply-from-block nil))
            ;; Apply
            (pcase mode
              ('replace
               ;; Snapshot before replace; GPTel apply will follow core (§02.8/§07).
               (when (fboundp 'context-navigator-snapshot-push)
                 (ignore-errors (context-navigator-snapshot-push)))
               (ignore-errors (context-navigator-set-items items)))
              ('add
               ;; Merge with current model; dedup; push only new/re-enabled to GPTel (§13.7).
               (let* ((st-before (and (fboundp 'context-navigator--state-get)
                                      (ignore-errors (context-navigator--state-get))))
                      (old (and st-before (ignore-errors (context-navigator-state-items st-before))))
                      (merged (context-navigator-model-uniq (append (or old '()) (or items '())))))
                 (ignore-errors (context-navigator-set-items merged))
                 (let* ((st-after (and (fboundp 'context-navigator--state-get)
                                       (ignore-errors (context-navigator--state-get))))
                        (items-before (and st-before (ignore-errors (context-navigator-state-items st-before))))
                        (items-after  (and st-after  (ignore-errors (context-navigator-state-items st-after))))
                        (diff (context-navigator-model-diff (or items-before '()) (or items-after '())))
                        (adds (plist-get diff :add)))
                   (when (fboundp 'context-navigator-path-add--maybe-apply-to-gptel)
                     (context-navigator-path-add--maybe-apply-to-gptel adds))))))
            ;; Summary
            (when (fboundp 'context-navigator-ui-info)
              (context-navigator-ui-info :ctxblk-summary files sels bufs skipped-total))))))))

;;;###autoload
(defun context-navigator-context-block-apply-add ()
  "If inside a context block, parse and add items to the current group."
  (interactive)
  (cn-ctxblk--apply-from-block 'add))

;;;###autoload
(defun context-navigator-context-block-apply-replace ()
  "If inside a context block, parse and replace items in the current group."
  (interactive)
  (cn-ctxblk--apply-from-block 'replace))

(provide 'context-navigator-context-blocks)
;;; context-navigator-context-blocks.el ends here
