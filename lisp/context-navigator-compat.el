;;; context-navigator-compat.el --- Small compatibility shims -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provide lightweight fallbacks for functions that may be missing on
;; some Emacs/CL builds used in CI or user setups.

;;; Code:

(require 'cl-lib)

;; Ensure project tracking vars are bound early to avoid void-variable errors
(unless (boundp 'context-navigator-project--last-root)
  (defvar context-navigator-project--last-root nil))
(unless (boundp 'context-navigator-project--last-switch-time)
  (defvar context-navigator-project--last-switch-time 0.0))

;; Soft-bind Stats split buffer to prevent early hook errors before the module is loaded.
(unless (boundp 'context-navigator-stats-split--buffer)
  (defvar context-navigator-stats-split--buffer nil))

;; Also soft-bind project hook flags/counters to survive partial/mixed loads
(unless (boundp 'context-navigator-project--hooks-installed)
  (defvar context-navigator-project--hooks-installed nil))
(unless (boundp 'context-navigator-project--hook-error-count)
  (defvar context-navigator-project--hook-error-count 0))
(unless (boundp 'context-navigator-project--hook-error-last-time)
  (defvar context-navigator-project--hook-error-last-time 0.0))

;; Provide a minimal `cl-copy-struct' fallback when absent.
(unless (fboundp 'cl-copy-struct)
  (defun cl-copy-struct (obj)
    "Fallback compatibility for `cl-copy-struct'.
For known structs, return a shallow copy; for vectors use `copy-sequence';
otherwise try `copy-tree' as a best-effort generic copy."
    (cond
     ;; Context Navigator's state struct (avoid depending on core at load time)
     ((and (fboundp 'context-navigator-state-p)
           (ignore-errors (context-navigator-state-p obj))
           (fboundp 'context-navigator--state-copy))
      (context-navigator--state-copy obj))
     ((vectorp obj)
      (copy-sequence obj))
     (t
      (copy-tree obj)))))

(provide 'context-navigator-compat)
;;; context-navigator-compat.el ends here
