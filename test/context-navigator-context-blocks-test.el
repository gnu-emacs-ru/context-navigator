;;; context-navigator-context-blocks-test.el --- Tests for Org context blocks -*- lexical-binding: t; -*-

(require 'ert)
(require 'context-navigator-model)
(require 'context-navigator-context-blocks)

(ert-deftest cn-ctxblk-parse-buffer-line ()
  "buf:<NAME> should yield a buffer item when NAME exists."
  (let ((buf (generate-new-buffer "*CN-CTXBLK*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            ;; Ensure buffer has the desired name and is live
            (rename-buffer "*CN-CTXBLK*" t))
          (let* ((body (format "buf:%s\n" (buffer-name buf)))
                 (res  (funcall (intern "cn-ctxblk--parse-lines") body)))
            (should (equal (plist-get res :buffers) 1))
            (should (equal (plist-get res :selections) 0))
            (should (equal (plist-get res :files) 0))
            (should (equal (plist-get res :skipped) 0))
            (should (equal (plist-get res :total) 1))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest cn-ctxblk-emit-block-mixed ()
  "Emission should produce a begin/end block and include mixed items."
  (let* ((tmp-file "/tmp/cn-ctxblk-a.txt")
         (file-it  (context-navigator-item-create
                    :type 'file :name "a.txt" :path tmp-file :enabled t))
         (buf (generate-new-buffer "*CN-CTXBLK-EMIT*"))
         (buf-it nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (rename-buffer "*CN-CTXBLK-EMIT*" t))
          (setq buf-it (context-navigator-item-create
                        :type 'buffer :name (buffer-name buf) :buffer buf :enabled t))
          (let* ((out (funcall (intern "cn-ctxblk--emit-block") (list file-it buf-it))))
            (should (string-prefix-p "#+begin_context" out))
            (should (string-suffix-p "#+end_context\n" out))
            (should (string-match (regexp-quote (format " %s" (expand-file-name tmp-file))) out))
            (should (string-match (regexp-quote (format " buf:%s" (buffer-name buf))) out))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(require 'org)
(require 'context-navigator-core)

(ert-deftest cn-ctxblk-parse-mixed-rel-abs ()
  "Parsing mixed absolute/relative paths and selections resolves via project root."
  (let* ((tmpdir (make-temp-file "cn-ctxblk" t))
         (abs (expand-file-name "a.txt" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file abs (insert "hi\n"))
          ;; Set project root in core state
          (let* ((st (context-navigator--state-get))
                 (new (copy-context-navigator-state st)))
            (setf (context-navigator-state-last-project-root new) tmpdir)
            (context-navigator--set-state new))
          (let* ((body (format " %s\n a.txt\n sel:a.txt:1-2\n" abs))
                 (res  (funcall (intern "cn-ctxblk--parse-lines") body)))
            (should (= (plist-get res :files) 2))
            (should (= (plist-get res :selections) 1))
            (should (= (plist-get res :buffers) 0))))
      (ignore-errors (delete-file abs))
      (ignore-errors (delete-directory tmpdir)))))

(ert-deftest cn-ctxblk-apply-add-replace ()
  "Apply add and replace from a context block at point inside an org buffer."
  (let* ((tmpdir (make-temp-file "cn-ctxblk" t))
         (f1 (expand-file-name "a.txt" tmpdir))
         (f2 (expand-file-name "b.txt" tmpdir))
         (context-navigator-autosave nil)) ;; avoid persist writes in tests
    (unwind-protect
        (progn
          (with-temp-file f1 (insert "a\n"))
          (with-temp-file f2 (insert "b\n"))
          ;; Set project root and clear model items
          (let* ((st (context-navigator--state-get))
                 (new (copy-context-navigator-state st)))
            (setf (context-navigator-state-last-project-root new) tmpdir)
            ;; keep current-group-slug nil to avoid autosave side effects
            (setf (context-navigator-state-current-group-slug new) nil)
            (context-navigator--set-state new)
            (context-navigator-set-items '()))
          ;; ADD from a mixed block: abs f1 + rel a.txt (dedup to one item)
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n"
                    (format " %s\n" f1)
                    " a.txt\n"
                    "#+end_context\n")
            ;; Put point inside block
            (goto-char (point-min))
            (search-forward "a.txt")
            (cl-letf (((symbol-function 'context-navigator-ui-ask)  (lambda (&rest _) t))
                      ((symbol-function 'context-navigator-ui-info) (lambda (&rest _) nil))
                      ((symbol-function 'context-navigator-ui-warn) (lambda (&rest _) nil)))
              (context-navigator-context-block-apply-add)))
          (let* ((st1 (context-navigator--state-get))
                 (items1 (context-navigator-state-items st1))
                 (files1 (cl-remove-if-not (lambda (it) (eq (context-navigator-item-type it) 'file)) items1)))
            (should (= (length files1) 1))
            (should (string= (expand-file-name (context-navigator-item-path (car files1)))
                             (expand-file-name f1))))
          ;; REPLACE with only b.txt
          (with-temp-buffer
            (org-mode)
            (insert "#+begin_context\n"
                    (format " %s\n" f2)
                    "#+end_context\n")
            (goto-char (point-min))
            (search-forward f2)
            (cl-letf (((symbol-function 'context-navigator-ui-ask)  (lambda (&rest _) t))
                      ((symbol-function 'context-navigator-ui-info) (lambda (&rest _) nil))
                      ((symbol-function 'context-navigator-ui-warn) (lambda (&rest _) nil)))
              (context-navigator-context-block-apply-replace)))
          (let* ((st2 (context-navigator--state-get))
                 (items2 (context-navigator-state-items st2))
                 (files2 (cl-remove-if-not (lambda (it) (eq (context-navigator-item-type it) 'file)) items2)))
            (should (= (length files2) 1))
            (should (string= (expand-file-name (context-navigator-item-path (car files2)))
                             (expand-file-name f2)))))
      (ignore-errors (delete-file f1))
      (ignore-errors (delete-file f2))
      (ignore-errors (delete-directory tmpdir)))))

(ert-deftest cn-ctxblk-filter-counters ()
  "Filter should count too-big and non-regular correctly."
  (let* ((tmpdir (make-temp-file "cn-ctxblk" t))
         (small (expand-file-name "s.txt" tmpdir))
         (big   (expand-file-name "b.txt" tmpdir))
         (ndir  (expand-file-name "adir" tmpdir))
         ;; Make size limit tiny to trigger too-big for BIG
         (context-navigator-max-file-size 1))
    (unwind-protect
        (progn
          (make-directory ndir)
          (with-temp-file small (insert "x"))
          (with-temp-file big   (insert "12345"))

          (let* ((small-it (context-navigator-item-create
                            :type 'file :name "s.txt" :path small :enabled t))
                 (big-it   (context-navigator-item-create
                            :type 'file :name "b.txt" :path big :enabled t))
                 ;; Deliberately construct a file item pointing to a directory (non-regular)
                 (dir-it   (context-navigator-item-create
                            :type 'file :name "adir"  :path ndir :enabled t))
                 (flt (funcall (intern "cn-ctxblk--filter-items") (list small-it big-it dir-it))))
            ;; Kept only small file
            (should (= (length (plist-get flt :items)) 1))
            (should (string= (expand-file-name (context-navigator-item-path (car (plist-get flt :items))))
                             (expand-file-name small)))
            ;; Counters
            (should (= (plist-get flt :skipped-too-big) 1))
            (should (= (plist-get flt :skipped-nonregular) 1))
            (should (= (plist-get flt :remote) 0))))
      (ignore-errors (delete-file small))
      (ignore-errors (delete-file big))
      (ignore-errors (delete-directory ndir))
      (ignore-errors (delete-directory tmpdir)))))

(ert-deftest cn-ctxblk-buf-degrade-to-file ()
  "buf:NAME:PATH should degrade to a file item when NAME is not a live buffer."
  (let* ((tmpdir (make-temp-file "cn-ctxblk" t))
         (abs (expand-file-name "a.txt" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file abs (insert "hi\n"))
          ;; Set project root in core state for relative resolution
          (let* ((st (context-navigator--state-get))
                 (new (copy-context-navigator-state st)))
            (setf (context-navigator-state-last-project-root new) tmpdir)
            (context-navigator--set-state new))
          ;; Non-existent buffer name + relative path -> file item
          (let* ((body (format " buf:%s:%s\n" "THIS-DOES-NOT-EXIST" "a.txt"))
                 (res  (funcall (intern "cn-ctxblk--parse-lines") body)))
            (should (= (plist-get res :files) 1))
            (should (= (plist-get res :buffers) 0))
            (should (= (plist-get res :selections) 0))))
      (ignore-errors (delete-file abs))
      (ignore-errors (delete-directory tmpdir)))))

(provide 'context-navigator-context-blocks-test)
;;; context-navigator-context-blocks-test.el ends here
