;;; context-navigator-view-buffer.el --- Buffer-mode (magit-like) UI -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Extracted buffer-mode helpers from context-navigator-view.el:
;; - context-navigator--buffer-mode--split
;; - context-navigator-buffer-open
;; - context-navigator-buffer-close
;; - context-navigator-buffer-toggle
;;
;; This module avoids requiring the full view to prevent load cycles and uses
;; declare-function for helpers provided by the view/windows modules.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'context-navigator-view-events)
(require 'context-navigator-view-constants)

;; Declarations from the main view/windows (avoid cycles)
(declare-function context-navigator-view-mode "context-navigator-view" ())
(declare-function context-navigator-view--render "context-navigator-view" ())




(defun context-navigator--buffer-mode--split (direction size)
  "Split selected window in DIRECTION ('right or 'below) using SIZE.

SIZE is interpreted as:
- 0 < SIZE < 1 → fraction of current window size
- SIZE >= 1     → absolute columns/rows depending on DIRECTION"
  (let* ((base (selected-window))
         (cols (window-total-width base))
         (rows (window-total-height base))
         (amt (cond
               ((and (numberp size) (> size 0) (< size 1))
                (if (eq direction 'right) (floor (* cols size)) (floor (* rows size))))
               ((and (numberp size) (>= size 1)) size)
               (t nil))))
    (split-window base amt direction)))

;;;###autoload
(defun context-navigator-buffer-open ()
  "Open the Navigator buffer in a regular window (magit-like).
- Reuse other window when available, else split (right by default).
- Always select the Navigator window."
  (interactive)
  (let* ((buf (get-buffer-create context-navigator-view--buffer-name))
         (visible (get-buffer-window buf nil))
         (placement (if (boundp 'context-navigator-buffer-placement)
                        context-navigator-buffer-placement
                      'reuse-other-window))
         (split-size (if (boundp 'context-navigator-buffer-split-size)
                         context-navigator-buffer-split-size
                       0.5))
         win)
    (if (window-live-p visible)
        (setq win visible)
      (pcase placement
        ('reuse-other-window
         (let* ((wins (seq-filter (lambda (w) (and (window-live-p w)
                                                   (not (eq w (selected-window)))))
                                  (window-list (selected-frame) 'no-minibuffer)))
                (w (car wins)))
           (if (window-live-p w)
               (setq win w)
             (setq win (context-navigator--buffer-mode--split 'right split-size)))))
        ('split-right
         (setq win (context-navigator--buffer-mode--split 'right split-size)))
        ('split-below
         (setq win (context-navigator--buffer-mode--split 'below split-size)))
        (_
         (setq win (context-navigator--buffer-mode--split 'right split-size)))))
    (when (window-live-p win)
      (set-window-buffer win buf)
      (when (window-live-p win)
        (set-window-parameter win 'context-navigator-view 'buffer))

      (with-current-buffer buf
        ;; Ensure major mode keymap exists even if view wasn't fully loaded yet
        (unless (and (boundp 'context-navigator-view-mode-map)
                     (keymapp context-navigator-view-mode-map))
          (setq context-navigator-view-mode-map (make-sparse-keymap)))
        (when (fboundp 'context-navigator-view-mode)
          (context-navigator-view-mode))
        (setq-local buffer-read-only t)
        (context-navigator-view-events-install)
        (when (fboundp 'context-navigator-view--render)
          (context-navigator-view--render)))
      (select-window win))
    win))

;;;###autoload
(defun context-navigator-buffer-close ()
  "Close Navigator buffer windows on the current frame (do not kill the buffer).

Also close any Navigator splits (Groups/Stats) if they are open."
  (interactive)
  (let* ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      ;; Close Groups/Stats splits first to avoid leaving orphan side windows.
      (ignore-errors
        (when (fboundp 'context-navigator-groups-split-close)
          (context-navigator-groups-split-close)))
      (ignore-errors
        (when (fboundp 'context-navigator-stats-split-close)
          (context-navigator-stats-split-close)))
      ;; Now close all windows that show the Navigator buffer in buffer-mode.
      (dolist (w (window-list (selected-frame) 'no-minibuffer))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf))
          (delete-window w))))))

;;;###autoload
(defun context-navigator-buffer-toggle ()
  "Toggle Navigator buffer visibility on the current frame."
  (interactive)
  (if (get-buffer-window context-navigator-view--buffer-name nil)
      (context-navigator-buffer-close)
    (context-navigator-buffer-open)))

(provide 'context-navigator-view-buffer)
;;; context-navigator-view-buffer.el ends here
