;;; context-navigator-diagnostics.el --- Diagnostics utilities (clean caches) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Small diagnostics helpers to clean byte-compiled (.elc) and native-compiled
;; (.eln) caches for Context Navigator and restart from a clean state.
;;
;; Useful after hot-reloads or mixed installs when symbol void/void-function
;; errors occur due to stale caches.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun context-navigator--diagnostics--repo-root ()
  "Return repository root (directory) or default-directory."
  (or (ignore-errors
        (let* ((dir (locate-dominating-file default-directory ".git")))
          (and dir (expand-file-name dir))))
      default-directory))

(defun context-navigator--diagnostics--delete-matching (dir predicate)
  "Delete files under DIR for which PREDICATE returns non-nil.
Silently ignore errors; return count of deleted files."
  (let ((deleted 0))
    (when (and (stringp dir) (file-directory-p dir))
      (dolist (f (directory-files-recursively dir ".*" t))
        (when (funcall predicate f)
          (ignore-errors
            (delete-file f)
            (setq deleted (1+ deleted))))))
    deleted))

(defun context-navigator--diagnostics--rm-elc-in (dir)
  "Delete all .elc files under DIR; return count."
  (context-navigator--diagnostics--delete-matching
   dir (lambda (f) (and (stringp f) (string-match-p "\\.elc\\'" f)))))

(defun context-navigator--diagnostics--rm-eln-like (dir)
  "Delete native .eln files in DIR that look related to Context Navigator; return count."
  (context-navigator--diagnostics--delete-matching
   dir (lambda (f)
         (and (stringp f)
              (string-match-p "\\.eln\\'" f)
              (string-match-p "context[-_]navigator" (file-name-nondirectory f))))))

;;;###autoload
(defun context-navigator-diagnostics-clean-caches ()
  "Delete Context Navigator .elc and .eln caches (safe) and report counts.

Performs:
- remove all .elc under the repository's lisp/ subtree
- remove elpa/straight build directories for context-navigator (best-effort)
- remove native eln-cache entries for context-navigator

This command does not touch non-Navigator files. Restart Emacs after running."
  (interactive)
  (let* ((root (context-navigator--diagnostics--repo-root))
         (lisp-dir (expand-file-name "lisp" root))
         (emacsd (or user-emacs-directory "~/.emacs.d"))
         (elpa (expand-file-name "elpa" emacsd))
         (straight (expand-file-name "straight/build" emacsd))
         (eln (expand-file-name "eln-cache" emacsd))
         (elc (context-navigator--diagnostics--rm-elc-in lisp-dir))
         (eln-deleted (context-navigator--diagnostics--rm-eln-like eln))
         (elpa-removed 0)
         (straight-removed 0))
    ;; Remove package manager build dirs (best-effort, quiet on errors)
    (dolist (dir (and (file-directory-p elpa)
                      (directory-files elpa t "\\`context[-_]navigator.*\\'")))
      (ignore-errors (delete-directory dir t))
      (setq elpa-removed (1+ elpa-removed)))
    (dolist (dir (and (file-directory-p straight)
                      (directory-files straight t "\\`context[-_]navigator.*\\'")))
      (ignore-errors (delete-directory dir t))
      (setq straight-removed (1+ straight-removed)))
    (message "[Context Navigator] Caches removed: elc=%d, eln=%d, elpa=%d, straight=%d. Please restart Emacs."
             elc eln-deleted elpa-removed straight-removed)
    (list :elc elc :eln eln-deleted :elpa elpa-removed :straight straight-removed)))

;;;###autoload
(defun context-navigator-diagnostics-hard-restart ()
  "Clean caches, unload Context Navigator features, and reload the umbrella.
Useful after hot-reloads or mixed installs to recover from stale .elc/.eln."
  (interactive)
  ;; Clean caches first (safe)
  (ignore-errors (context-navigator-diagnostics-clean-caches))
  ;; Unload all context-navigator* features to avoid stale symbols/advice/timers
  (dolist (feat (copy-sequence features))
    (when (and (symbolp feat)
               (string-prefix-p "context-navigator" (symbol-name feat)))
      (ignore-errors (unload-feature feat t))))
  (garbage-collect)
  ;; Reload umbrella entry
  (require 'context-navigator)
  (message "[Context Navigator] Hard restart done."))

(provide 'context-navigator-diagnostics)
;;; context-navigator-diagnostics.el ends here
