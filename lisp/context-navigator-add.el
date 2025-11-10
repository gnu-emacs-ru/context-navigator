;;; context-navigator-add.el --- Add logic for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core logic for adding files/regions/buffers into the Context Navigator model.
;; This module contains small, focused functions and interactive entry points
;; unrelated to transient UI assembly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dired)

(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-i18n)
(require 'context-navigator-ui)

(defgroup context-navigator-add nil
  "Settings for universal add operations."
  :group 'context-navigator)

(defcustom context-navigator-max-file-size (* 1 1024 1024)
  "Maximum file size (bytes) to include when adding files recursively.
Files larger than this threshold are skipped."
  :type 'integer :group 'context-navigator-add)

;; ---------------- Filesystem helpers ----------------

(defun context-navigator-add-regular-file-p (path)
  "Return non-nil if PATH is a regular file."
  (and (stringp path)
       (file-exists-p path)
       (file-regular-p path)))

(defun context-navigator-add-file-size (path)
  "Return size of PATH in bytes or nil."
  (when (and (stringp path) (file-exists-p path))
    (let ((attrs (file-attributes path 'string)))
      (and attrs (file-attribute-size attrs)))))

(defun context-navigator-add-collect-recursive (dir)
  "Collect regular files under DIR (recursive, robust across Emacs versions).

This normalizes DIR to an absolute directory and then attempts two strategies:
1. Use =directory-files-recursively' with a proper PREDICATE argument when
   available (Emacs 28+). Note the predicate is the 5th argument, so we pass
   two nils before it to keep argument positions correct.
2. Fallback to a plain =directory-files-recursively' call or, if that fails,
   return an empty list.

Always return a list of regular files (absolute paths) or '() on error."
  (let ((dir (and (stringp dir) (expand-file-name dir))))
    (if (not (and dir (file-directory-p dir)))
        '()
      (let* ((rx ".*")
             (all
              (or
               ;; Correct call: DIR RX INCLUDE-DIRECTORIES FOLLOW-SYMLINKS PREDICATE
               (ignore-errors (directory-files-recursively dir rx nil nil #'file-regular-p))
               ;; Older Emacs: fallback without PREDICATE
               (ignore-errors (directory-files-recursively dir rx))
               '())))
        (if (listp all)
            (cl-remove-if-not #'file-regular-p all)
          '())))))

(defun context-navigator-add-gather-files (paths)
  "From PATHS (files/dirs), return plist:
(:files L :skipped-too-big N :skipped-nonregular M :remote K)."
  (let ((files '())
        (skipped-too-big 0)
        (skipped-nonregular 0)
        (remote 0)
        (limit (or context-navigator-max-file-size most-positive-fixnum)))
    (dolist (p paths)
      (cond
       ((and (stringp p) (file-directory-p p))
        (dolist (f (context-navigator-add-collect-recursive p))
          (let ((sz (context-navigator-add-file-size f)))
            (when (file-remote-p f) (setq remote (1+ remote)))
            (cond
             ((null sz) (setq skipped-nonregular (1+ skipped-nonregular)))
             ((> sz limit) (setq skipped-too-big (1+ skipped-too-big)))
             (t (push f files))))))
       ((context-navigator-add-regular-file-p p)
        (let ((sz (context-navigator-add-file-size p)))
          (when (file-remote-p p) (setq remote (1+ remote)))
          (if (and sz (> sz limit))
              (setq skipped-too-big (1+ skipped-too-big))
            (push p files))))
       (t
        (setq skipped-nonregular (1+ skipped-nonregular)))))
    (list :files (nreverse (delete-dups files))
          :skipped-too-big skipped-too-big
          :skipped-nonregular skipped-nonregular
          :remote remote)))

;; ---------------- Preview UI ----------------

(defun context-navigator-add-preview-and-confirm (files stats)
  "Show preview buffer for FILES and STATS, return non-nil to proceed."
  (let* ((buf (get-buffer-create "*Context Navigator Add Preview*"))
         (total (length files))
         (too-big (plist-get stats :skipped-too-big))
         (nonreg (plist-get stats :skipped-nonregular))
         (remote (plist-get stats :remote))
         (sum-bytes (cl-loop for f in files
                             for s = (context-navigator-add-file-size f)
                             when s sum s)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format (context-navigator-i18n :preview-title)
                        total
                        (context-navigator-human-size sum-bytes)))
        (insert "\n")
        (when (> too-big 0)
          (insert (format (context-navigator-i18n :preview-skipped-too-big)
                          too-big
                          (context-navigator-human-size context-navigator-max-file-size)))
          (insert "\n"))
        (when (> nonreg 0)
          (insert (format (context-navigator-i18n :preview-skipped-nonregular) nonreg))
          (insert "\n"))
        (when (> remote 0)
          (insert (format (context-navigator-i18n :preview-remote) remote))
          (insert "\n"))
        (insert "\n")
        (insert (context-navigator-i18n :preview-files))
        (insert "\n")
        (dolist (f files)
          (insert (format "  %s (%s)\n"
                          (abbreviate-file-name f)
                          (context-navigator-human-size (context-navigator-add-file-size f)))))
        (goto-char (point-min))
        (view-mode 1)))
    (save-window-excursion
      (display-buffer
       buf
       '((display-buffer-in-side-window)
         (side . bottom)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (no-delete-other-windows . t)))))
      (unwind-protect
          (context-navigator-ui-ask :confirm-add total)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; ---------------- Pure-ish builders ----------------

(defun context-navigator-add--normalize-paths (files)
  "Return list of absolute paths from FILES, dropping non-strings."
  (delq nil (mapcar (lambda (p) (and (stringp p) (expand-file-name p))) files)))

(defun context-navigator-add--build-file-item (path)
  "Create a file item for PATH (absolute)."
  (when (and (stringp path) (file-exists-p path) (not (file-directory-p path)))
    (context-navigator-item-create
     :type 'file
     :name (file-name-nondirectory path)
     :path (expand-file-name path)
     :enabled t)))

(defun context-navigator-add--dedupe-old-items (old-items abs-hash)
  "Return OLD-ITEMS filtered to drop any that reference ABS-HASH keys by path."
  (cl-remove-if
   (lambda (it)
     (let* ((p (context-navigator-item-path it))
            (bp (and (bufferp (context-navigator-item-buffer it))
                     (buffer-live-p (context-navigator-item-buffer it))
                     (buffer-local-value 'buffer-file-name (context-navigator-item-buffer it))))
            (pp (and (stringp p) (expand-file-name p)))
            (bb (and (stringp bp) (expand-file-name bp))))
       (or (and pp (gethash pp abs-hash))
           (and bb (gethash bb abs-hash)))))
   (or old-items '())))

(defun context-navigator-add--merge-files-into-items (old-items files)
  "Return cons (MERGED . NEW-OR-ENABLED) by merging FILES into OLD-ITEMS.
If a file item already exists (same stable key), re-enable it instead of duplicating.
NEW-OR-ENABLED contains items that should be applied to gptel immediately (new or re-enabled)."
  (let* ((abs (context-navigator-add--normalize-paths files))
         ;; Absolute-path set: drop any old items that reference these file paths
         (abs-set (let ((h (make-hash-table :test 'equal)))
                    (dolist (p abs) (puthash p t h)) h))
         ;; Current index (key -> item) to locate existing entries quickly
         (st  (ignore-errors (context-navigator--state-get)))
         (idx (and st (context-navigator-state-index st)))
         (keys-to-replace (make-hash-table :test 'equal))
         (apply-items '())
         (replacements '()))
    ;; For each requested file path decide: re-enable existing or create new
    (dolist (p abs)
      (let* ((tmp (context-navigator-item-create :type 'file
                                                 :name (file-name-nondirectory p)
                                                 :path p :enabled t))
             (key (context-navigator-model-item-key tmp))
             (existing (and (hash-table-p idx) (gethash key idx))))
        (if (context-navigator-item-p existing)
            ;; Re-enable existing item when needed; keep all other fields
            (let* ((en (context-navigator-item-enabled existing))
                   (upd (if en
                            existing
                          (context-navigator-item-create
                           :type (context-navigator-item-type existing)
                           :name (context-navigator-item-name existing)
                           :path (context-navigator-item-path existing)
                           :buffer (context-navigator-item-buffer existing)
                           :beg (context-navigator-item-beg existing)
                           :end (context-navigator-item-end existing)
                           :size (context-navigator-item-size existing)
                           :enabled t
                           :meta (context-navigator-item-meta existing)))))
              (puthash key t keys-to-replace)
              (when (context-navigator-item-p upd)
                (push upd replacements))
              ;; Only schedule for apply when we flipped enabled from nil -> t
              (when (and (context-navigator-item-p upd) (not en))
                (push upd apply-items)))
          ;; No existing item -> create a new enabled file item
          (let ((it (context-navigator-add--build-file-item p)))
            (when it
              (push it replacements)
              (push it apply-items))))))
    ;; Keep old items except ones replaced by key or path
    (let* ((keep
            (cl-remove-if
             (lambda (it)
               (let* ((p (and (stringp (context-navigator-item-path it))
                              (expand-file-name (context-navigator-item-path it))))
                      (key (context-navigator-model-item-key it)))
                 (or (and p (gethash p abs-set))
                     (and key (gethash key keys-to-replace)))))
             (or old-items '())))
           (merged (append keep (nreverse replacements))))
      (cons merged (nreverse apply-items)))))

;; ---------------- gptel batched apply helpers ----------------

(defun context-navigator-add--refresh-gptel-keys-snapshot ()
  "Refresh cached gptel keys snapshot in Navigator buffer to update indicators."
  (ignore-errors
    (let* ((lst (context-navigator-gptel-pull))
           (keys (and (listp lst)
                      (mapcar #'context-navigator-model-item-key lst)))
           (h (sxhash-equal keys)))
      (with-current-buffer (get-buffer-create "*context-navigator*")
        (setq-local context-navigator-view--gptel-keys keys)
        (setq-local context-navigator-view--gptel-keys-hash h))
      (ignore-errors
        (when (fboundp 'context-navigator-debug)
          (context-navigator-debug :debug :ui
                                   "add: gptel-keys snapshot set: %d (h=%s)"
                                   (length (or keys '())) h)))
      ;; Immediately request a lightweight UI refresh so indicator lamps update now.
      (when (fboundp 'context-navigator-view--schedule-render-soft)
        (context-navigator-view--schedule-render-soft))
      (when (fboundp 'context-navigator-view--render-if-visible)
        (context-navigator-view--render-if-visible)))))

(defun context-navigator-add--ui-refresh-now (&optional invalidate)
  "Force a quick Navigator UI refresh; when INVALIDATE is non-nil, drop render caches."
  (ignore-errors
    (let ((buf (get-buffer "*context-navigator*")))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when invalidate
            (setq-local context-navigator-render--last-hash nil)
            (setq-local context-navigator-view--last-render-key nil)
            (setq-local context-navigator-controls--cache-key nil)
            (setq-local context-navigator-controls--cache-str nil))
          (when (fboundp 'context-navigator-view--render-if-visible)
            (context-navigator-view--render-if-visible))
          (when (fboundp 'context-navigator-debug)
            (context-navigator-debug :trace :ui
                                     "add: ui-refresh-now invalidate=%s"
                                     (and invalidate t))))))))

(defun context-navigator-add--apply-items-batched (items)
  "Background-apply ITEMS to gptel via core batch when push is ON."
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel
             (listp items))
    (let* ((st (context-navigator--state-get))
           (token (and st (context-navigator-state-load-token st))))
      (ignore-errors
        (let ((context-navigator-gptel-require-visible-window nil))
          (context-navigator--gptel-defer-or-start items token))))
    ;; Best-effort immediate refresh of indicators; events will refine it later.
    (run-at-time 0.1 nil #'context-navigator-add--refresh-gptel-keys-snapshot)))

;; ---------------- Mutators (model + apply + UI) ----------------

(defun context-navigator-add-files (files)
  "Add FILES (list of paths) to the model, deduping by absolute path.
Replace any existing items that reference the same files. Apply to gptel (batched) when enabled."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (old (and (context-navigator-state-p st) (context-navigator-state-items st)))
         (res (context-navigator-add--merge-files-into-items old files))
         (merged (car res))
         (new-items (cdr res)))
    (context-navigator-set-items merged)
    (ignore-errors
      (when (fboundp 'context-navigator-debug)
        (context-navigator-debug :debug :add
                                 "add-files: merged=%d apply=%d"
                                 (length (or merged '()))
                                 (length (or new-items '())))))
    ;; Force a quick UI refresh so lamps can update immediately; invalidate caches to avoid skips.
    (context-navigator-add--ui-refresh-now t)
    (context-navigator-add--apply-items-batched new-items)
    (context-navigator-ui-info :added-files (length new-items))))

;; ---------------- Universal add dispatch helpers ----------------

(defun context-navigator-add--region-active-p ()
  "Return non-nil only when there is a real, visible active region."
  (use-region-p))

;; --- Context-aware helpers (selection/file at point) ------------------------

(defun context-navigator-add--find-selection-containing-point ()
  "Return selection item at point in current buffer, or nil.
Matches items of type 'selection whose path/buffer corresponds to the current
buffer and whose [beg,end] range contains point."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st)))
         (buf (current-buffer))
         (path (buffer-file-name buf))
         (pt (point)))
    (cl-find-if
     (lambda (it)
       (and (eq (context-navigator-item-type it) 'selection)
            (let* ((it-buf (context-navigator-item-buffer it))
                   (it-path (context-navigator-item-path it))
                   (beg (context-navigator-item-beg it))
                   (end (context-navigator-item-end it))
                   (lo (and (integerp beg) (integerp end) (min beg end)))
                   (hi (and (integerp beg) (integerp end) (max beg end)))
                   (same-buf (or (and (buffer-live-p it-buf) (eq it-buf buf))
                                 (and (stringp it-path) (stringp path)
                                      (file-equal-p it-path path)))))
              (and same-buf lo hi (<= lo pt) (<= pt hi)))))
     (or items '()))))

(defun context-navigator-add--find-file-item-for-current-buffer ()
  "Return file item for the current buffer if present in the model, or nil."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (idx (and st (context-navigator-state-index st)))
         (path (buffer-file-name (current-buffer))))
    (when (and (hash-table-p idx) (stringp path))
      (let* ((key (context-navigator-model-item-key
                   (context-navigator-item-create :type 'file
                                                  :name (file-name-nondirectory path)
                                                  :path (expand-file-name path))))
             (it (gethash key idx)))
        (and (context-navigator-item-p it)
             (eq (context-navigator-item-type it) 'file)
             it)))))

;;;###autoload
(defun context-navigator-remove-here ()
  "Remove the selection/file at point from the context.
Priority:
- If point is inside an added selection, remove that selection item.
- Otherwise, if current buffer's file is in the context, remove that file item."
  (interactive)
  (let* ((it (or (context-navigator-add--find-selection-containing-point)
                 (context-navigator-add--find-file-item-for-current-buffer))))
    (if (not (context-navigator-item-p it))
        (ignore-errors (context-navigator-ui-info :nothing-to-delete))
      ;; Push snapshot for Undo/Redo before changing the model
      (when (fboundp 'context-navigator-snapshot-push)
        (ignore-errors (context-navigator-snapshot-push)))
      (let* ((key (context-navigator-model-item-key it)))
        (ignore-errors (context-navigator-remove-item-by-key key))
        (ignore-errors
          (context-navigator-ui-info :deleted-from-model
                                     (or (context-navigator-item-name it) key)))
        ;; Soft UI refresh (if sidebar is open)
        (when (fboundp 'context-navigator-view--render-if-visible)
          (context-navigator-view--render-if-visible))))))

;;;###autoload
(defun context-navigator-disable-here ()
  "Disable the selection/file at point in the context (keep it listed but off).
Priority:
- If point is inside an added selection, disable that selection item.
- Otherwise, if current buffer's file is in the context, disable that file item."
  (interactive)
  (let* ((it (or (context-navigator-add--find-selection-containing-point)
                 (context-navigator-add--find-file-item-for-current-buffer))))
    (if (not (context-navigator-item-p it))
        (ignore-errors (context-navigator-ui-info :no-items-in-context))
      ;; Push snapshot for Undo/Redo before changing the model
      (when (fboundp 'context-navigator-snapshot-push)
        (ignore-errors (context-navigator-snapshot-push)))
      (let* ((key (context-navigator-model-item-key it)))
        (ignore-errors (context-navigator-toggle-item key nil))
        ;; Soft UI refresh (if sidebar is open)
        (when (fboundp 'context-navigator-view--render-if-visible)
          (context-navigator-view--render-if-visible))))))


(defun context-navigator-add--dired-selection ()
  "Handle Dired selection for universal add."
  (let* ((sel (dired-get-marked-files nil nil)))
    (if (null sel)
        (context-navigator-ui-info :no-files-selected)
      (let* ((has-dir (cl-some #'file-directory-p sel))
             (stats (context-navigator-add-gather-files sel))
             (files (plist-get stats :files))
             (remote (plist-get stats :remote)))
        (if has-dir
            (progn
              (if (and (> remote 0)
                       (not (context-navigator-ui-ask :warn-remote-selected remote)))
                  (context-navigator-ui-info :aborted)
                (if (context-navigator-add-preview-and-confirm files stats)
                    (context-navigator-add-files files)
                  (context-navigator-ui-info :aborted))))
          (context-navigator-add-files files))))))

(defun context-navigator-add--add-selection-from-region ()
  "Create and add a selection item from the active region."
  (if (not (use-region-p))
      (context-navigator-ui-info :no-active-region)
    (let* ((buf (current-buffer))
           (p   (buffer-file-name buf))
           (beg (region-beginning))
           (end (region-end))
           (nm (if p
                   (format "%s:%s-%s" (file-name-nondirectory p) beg end)
                 (format "%s:%s-%s" (buffer-name buf) beg end)))
           (sel (context-navigator-item-create
                 :type 'selection :name nm
                 :path p :buffer buf :beg beg :end end :enabled t)))
      (ignore-errors (context-navigator-add-item sel))
      ;; Fully clear region so the next call won't treat a stale mark as selection
      (ignore-errors (deactivate-mark t))
      (ignore-errors (set-marker (mark-marker) nil (current-buffer)))
      ;; Apply only the selection (tests expect selection to be primary)
      (context-navigator-add--apply-items-batched (list sel))
      (context-navigator-ui-info :added-selection))))

(defun context-navigator-add--add-current-file ()
  "Add the current file-backed buffer as a file item."
  (let* ((p (buffer-file-name (current-buffer))))
    (if (file-remote-p p)
        (if (context-navigator-ui-ask :warn-remote-current)
            (context-navigator-add-files (list p))
          (context-navigator-ui-info :aborted))
      (context-navigator-add-files (list p)))))

(defun context-navigator-add--add-current-buffer ()
  "Add the current buffer as a buffer item."
  (let* ((b (current-buffer))
         (it (context-navigator-item-create
              :type 'buffer :name (buffer-name b) :buffer b :enabled t)))
    (ignore-errors (context-navigator-add-item it))
    (context-navigator-add--apply-items-batched (list it))
    (context-navigator-ui-info :added-buffer)))

;;;###autoload
(defun context-navigator-add-universal ()
  "Add current selection/file/buffer or Dired selection to the context.

Behavior:
- Dired:
  - If selection includes directories, collect files recursively with preview + confirmation.
  - Otherwise add marked files (filtering by max size).
- Active region: add as selection.
- File-backed buffer (no region): add file.
- Otherwise: add whole buffer (as buffer item).

TRAMP/remote: show a warning and confirm before proceeding."
  (interactive)
  (cond
   ;; If no region and point is inside a context block, apply items from the block (add/merge).
   ((and (not (context-navigator-add--region-active-p))
         (ignore-errors (require 'context-navigator-context-blocks nil t))
         (fboundp 'cn-ctxblk--block-text-at-point)
         (cn-ctxblk--block-text-at-point))
    (context-navigator-context-block-apply-add))
   ((derived-mode-p 'dired-mode)
    (context-navigator-add--dired-selection))
   ((context-navigator-add--region-active-p)
    (context-navigator-add--add-selection-from-region))
   ((buffer-file-name (current-buffer))
    (context-navigator-add--add-current-file))
   (t
    (context-navigator-add--add-current-buffer))))

(provide 'context-navigator-add)
;;; context-navigator-add.el ends here
