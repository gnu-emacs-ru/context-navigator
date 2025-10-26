;;; context-navigator-icons.el --- Optional icon provider -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Optional icons provider using all-the-icons when available.
;; Returns small, cached strings; safe to disable on remote/TRAMP.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)

(defgroup context-navigator-icons nil
  "Icon settings for context-navigator."
  :group 'context-navigator)

(defcustom context-navigator-enable-icons t
  "Whether to render icons in the sidebar."
  :type 'boolean :group 'context-navigator-icons)

(defcustom context-navigator-icons-disable-on-remote t
  "Disable icons on remote/TRAMP buffers/paths."
  :type 'boolean :group 'context-navigator-icons)

(defcustom context-navigator-icons-icon-height 0.65
  "Default height scaling for item icons (file/buffer/selection) in the Navigator."
  :type 'number :group 'context-navigator-icons)

(defcustom context-navigator-icons-icon-raise -0.06
  "Vertical raise for item icons to visually center them with text."
  :type 'number :group 'context-navigator-icons)

(defvar context-navigator-icons--cache (make-hash-table :test 'equal))

(defun context-navigator-icons-clear-cache ()
  "Clear cached icons."
  (clrhash context-navigator-icons--cache))

(defun context-navigator-icons--remote-p (item)
  "Return non-nil if ITEM path is remote."
  (let ((p (context-navigator-item-path item)))
    (and (stringp p) (file-remote-p p))))

(defun context-navigator-icons--ensure ()
  "Return non-nil if icons can be provided."
  (and context-navigator-enable-icons
       (fboundp 'all-the-icons-icon-for-file)))

(defun context-navigator-icons-for-item (item)
  "Return an icon string for ITEM or nil (slightly reduced size to keep rows compact)."
  (when (and (context-navigator-icons--ensure)
             (not (and context-navigator-icons-disable-on-remote
                       (context-navigator-icons--remote-p item))))
    (let* ((key (list (context-navigator-item-type item)
                      (or (context-navigator-item-path item) "")
                      (or (context-navigator-item-name item) "")
                      context-navigator-icons-icon-height
                      context-navigator-icons-icon-raise))
           (cached (gethash key context-navigator-icons--cache)))
      (or cached
          (let* ((type (context-navigator-item-type item))
                 (raw
                  (pcase type
                    ('file (when-let* ((p (context-navigator-item-path item))
                                       (ext (file-name-extension p)))
                             (ignore-errors (all-the-icons-icon-for-file p))))
                    ('buffer (ignore-errors (all-the-icons-octicon "file-text")))
                    ('selection (ignore-errors (all-the-icons-material "content_copy")))
                    (_ nil))))
            (when (stringp raw)
              ;; Apply configured icon height and vertical raise to visually center with filename
              (let* ((face-prop (get-text-property 0 'face raw))
                     (height-plist (list :height context-navigator-icons-icon-height))
                     (new-face (cond
                                ((null face-prop) height-plist)
                                ((symbolp face-prop) (list face-prop height-plist))
                                ((and (listp face-prop) (keywordp (car face-prop)))
                                 (list (append face-prop height-plist)))
                                ((listp face-prop) (append face-prop (list height-plist)))
                                (t height-plist)))
                     (icon (propertize raw
                                       'face new-face
                                       'display (list 'raise context-navigator-icons-icon-raise))))
                (puthash key icon context-navigator-icons--cache)
                icon)))))))

(defun context-navigator-icons-for-indicator (state)
  "Return a small icon string for indicator STATE or nil.
STATE is one of: 'ok, 'mismatch, 'absent.

Icons are rendered smaller and slightly raised so they align vertically
with surrounding item text and appear visually centered."
  (let* ((color (pcase state
                  ('ok "green4")
                  ('mismatch "goldenrod2")
                  (_ "gray")))
         (icon
          (cond
           ;; Prefer Font Awesome if available
           ((fboundp 'all-the-icons-faicon)
            (pcase state
              ('ok (all-the-icons-faicon "circle"))
              ('absent (all-the-icons-faicon "circle-o"))
              ('mismatch (all-the-icons-faicon "dot-circle-o"))))
           ;; Material fallback if faicon missing
           ((fboundp 'all-the-icons-material)
            (pcase state
              ('ok (all-the-icons-material "lens"))
              ('absent (all-the-icons-material "panorama_fish_eye"))
              ('mismatch (all-the-icons-material "brightness_1"))))
           (t nil))))
    (when (stringp icon)
      (propertize icon
                  'face (list :foreground color :height 0.5)
                  ;; Small raise to visually center the icon relative to item text.
                  'display '(raise 0.3)))))

;; Unified UI indicator (present/absent) --------------------------------------

(defun context-navigator-indicator-string (present &optional prefer-icons)
  "Return a small indicator string for PRESENT state.
When PREFER-ICONS is non-nil and an icon provider is available, use icons;
otherwise fall back to a colored text bullet.

- present → green ● (or [X] in TTY when text style)
- absent  → gray  ○ (or [ ] in TTY when text style)

The visual size and vertical alignment of text bullets are adjusted so the
indicator sits centered and appears moderately large relative to item text.
In non-graphical (TTY/batch) displays and when PREFER-ICONS is nil (text style),
return ASCII checkboxes: [X] or [ ]."
  (let* ((tty (not (display-graphic-p)))
         (use-icons (and prefer-icons (fboundp 'context-navigator-icons-for-indicator)))
         (state (if present 'ok 'absent)))
    (cond
     ;; Text style in TTY/batch: use ASCII checkboxes
     ((and (not use-icons) tty)
      (if present "[X]" "[ ]"))

     ;; Icons preferred: try icons; fallback to colored bullets
     (use-icons
      (let ((icon (ignore-errors (context-navigator-icons-for-indicator state))))
        (if (and (stringp icon) (> (length icon) 0))
            icon
          (let* ((raw (if present "●" "○"))
                 (color (if present "green4" "gray")))
            (propertize raw
                        'face (list :foreground color :height 0.75)
                        'display '(raise 0.08))))))

     ;; Text bullets (graphical or when icons not preferred)
     (t
      (let* ((raw (if present "●" "○"))
             (color (if present "green4" "gray")))
        (propertize raw
                    'face (list :foreground color :height 0.75)
                    'display '(raise 0.08)))))))

(provide 'context-navigator-icons)
;;; context-navigator-icons.el ends here
