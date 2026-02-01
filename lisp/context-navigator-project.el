;;; context-navigator-project.el --- Project detection and switch events -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Detect project roots using project.el or projectile (soft deps)
;; and publish :project-switch events on meaningful changes.
;; Filters out uninteresting buffers (e.g., Dired).
;;
;; Side-effects are limited to hook setup/teardown and event publishing.

;;; Code:

(require 'context-navigator-compat)

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-events)

;; Defensive: ensure internal vars are always bound even under partial loads
;; (e.g., mixed/old .elc) so hooks don't error-storm and freeze Emacs.
(defvar context-navigator-project--hooks-installed nil)
(defvar context-navigator-project--last-root nil)
(defvar context-navigator-project--last-switch-time 0.0)
(defvar context-navigator-project--hook-error-count 0)
(defvar context-navigator-project--hook-error-last-time 0.0)

(defun context-navigator-project--ensure-vars ()
  "Ensure internal tracking vars are bound (defensive against partial/old loads)."
  (unless (boundp 'context-navigator-project--hooks-installed)
    (defvar context-navigator-project--hooks-installed nil))
  (unless (boundp 'context-navigator-project--last-root)
    (defvar context-navigator-project--last-root nil))
  (unless (boundp 'context-navigator-project--last-switch-time)
    (defvar context-navigator-project--last-switch-time 0.0))
  (unless (boundp 'context-navigator-project--hook-error-count)
    (defvar context-navigator-project--hook-error-count 0))
  (unless (boundp 'context-navigator-project--hook-error-last-time)
    (defvar context-navigator-project--hook-error-last-time 0.0)))

(defun context-navigator-project--handle-hook-error (err where)
  "Handle ERR from a project hook WHERE, throttling and disabling hooks on bursts."
  (context-navigator-project--ensure-vars)
  (let* ((now (float-time))
         (dt (- now (or context-navigator-project--hook-error-last-time 0.0))))
    (setq context-navigator-project--hook-error-last-time now)
    (setq context-navigator-project--hook-error-count
          (if (< dt 1.0)
              (1+ (or context-navigator-project--hook-error-count 0))
            1))
    ;; If we error repeatedly, disable hooks to keep Emacs responsive.
    (when (>= (or context-navigator-project--hook-error-count 0) 5)
      (setq context-navigator-project--hook-error-count 0)
      (ignore-errors (context-navigator-project-teardown-hooks))
      (message "[context-navigator] project hooks disabled after repeated errors (%s): %S"
               where err))))

(defun context-navigator-project--now () (float-time))

(defgroup context-navigator-project nil
  "Project detection settings for Context Navigator."
  :group 'context-navigator)

(defcustom context-navigator-project-nonfile-modes
  '(gptel-mode comint-mode term-mode vterm-mode eshell-mode shell-mode ansi-term eat-mode
               dired-mode wdired-mode)
  "List of non-file major modes that may represent a real project context.
Buffers in these modes are considered interesting when their default-directory
resolves to a project root."
  :type '(repeat symbol)
  :group 'context-navigator-project)

(defcustom context-navigator-project-stick-to-last-root t
  "When non-nil, ignore transient automatic transitions to global (nil root).
Keeps the last non-nil project root until a new valid root is detected."
  :type 'boolean
  :group 'context-navigator-project)

(defun context-navigator-project--corfu-buffer-p (&optional buffer)
  "Return non-nil if BUFFER looks like a corfu internal buffer."
  (let ((b (or buffer (current-buffer))))
    (and (buffer-live-p b)
         (let ((name (buffer-name b)))
           (and (stringp name)
                (or (string-prefix-p " *corfu" name)
                    (string-prefix-p " *Corfu" name)))))))

(defun context-navigator-project--child-frame-p (&optional frame)
  "Return non-nil when FRAME (or selected) is a child frame (posframe/popups)."
  (let ((f (or frame (selected-frame))))
    (and (frame-live-p f)
         (frame-parameter f 'parent-frame))))

(defun context-navigator-project--writable-root-p (dir)
  "Return non-nil if DIR allows creating a subdirectory."
  (let* ((abs (and dir (directory-file-name (expand-file-name dir)))))
    (cond
     ((null abs) nil)
     ((file-directory-p abs) (file-writable-p abs))
     (t (let ((parent (file-name-directory abs)))
          (and parent (file-writable-p parent)))))))

(defun context-navigator-project-root (&optional buffer)
  "Return absolute project root for BUFFER (or current) or nil.

This attempts several strategies for robustness:
- Prefer `project-current' / `project-root' in the buffer itself.
- Fallback: if that fails, try detecting the project using the buffer's
  `default-directory' (useful for special buffers like gptel that are not
  file-backed but inherit a directory from their originating project).
- Finally, try projectile fallbacks with the same approaches.

Note: returns only local, writable roots to avoid switching to non-writable locations."
  (let* ((buf (or buffer (current-buffer)))
         ;; buffer's default-directory may be the best hint for non-file buffers
         (buf-dir (when (bufferp buf) (with-current-buffer buf default-directory)))
         (root
          (or
           ;; 1) Prefer project.el detection in the buffer context
           (when (fboundp 'project-current)
             (let ((proj (with-current-buffer buf (project-current nil))))
               (when proj (expand-file-name (project-root proj)))))
           ;; 2) Fallback: try project.el by treating buf-dir as default-directory
           (when (and (fboundp 'project-current) buf-dir)
             (let ((default-directory buf-dir))
               (when-let ((proj (project-current nil)))
                 (expand-file-name (project-root proj)))))
           ;; 3) projectile in buffer context (if available)
           (when (fboundp 'projectile-project-root)
             (with-current-buffer buf
               (ignore-errors (projectile-project-root))))
           ;; 4) projectile fallback using buf-dir
           (when (and (fboundp 'projectile-project-root) buf-dir)
             (let ((default-directory buf-dir))
               (ignore-errors (projectile-project-root)))))))
    (when (and (stringp root) (not (string-empty-p root)))
      (let ((abs (directory-file-name (expand-file-name root))))
        (and (context-navigator-project--writable-root-p abs) abs)))))

(defun context-navigator-project--interesting-buffer-p-impl (buffer)
  "Return non-nil if BUFFER should trigger project switching.

Considers:
- file-backed buffers;
- non-file interactive modes (gptel, comint/term/eshell/vterm/shell, etc.)
  when their default-directory resolves to a project root.

Excludes minibuffers, child-frame popups and corfu internal buffers."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (not (minibufferp buffer))
              (not (context-navigator-project--corfu-buffer-p buffer))
              (or buffer-file-name
                  (let* ((is-nonfile
                          (cl-some (lambda (m) (derived-mode-p m))
                                   context-navigator-project-nonfile-modes)))
                    (and is-nonfile
                         (context-navigator-project-root buffer))))))))
(defalias 'context-navigator-project--interesting-buffer-p
  'context-navigator-project--interesting-buffer-p-impl)

(defun context-navigator-project--maybe-publish-switch (&optional buffer)
  "Publish :project-switch only when the project root actually changes.

Throttled by `context-navigator-context-switch-interval' to avoid publishing
a flurry of events during rapid buffer/window changes. When a change is
detected but the last publish was too recent, schedule a debounced publish
for the remaining interval.

Sticky policy: when `context-navigator-project-stick-to-last-root' is non-nil,
do not auto-switch to global (nil root) on transient glitches — keep the last
valid root."
  (let* ((buf (or buffer (current-buffer))))
    (when (context-navigator-project--interesting-buffer-p buf)
      (let* ((computed (context-navigator-project-root buf))
             (root (if (or computed (not context-navigator-project-stick-to-last-root))
                       computed
                     ;; computed is nil and sticky is enabled → stick to last-root
                     context-navigator-project--last-root)))
        (unless (equal root context-navigator-project--last-root)
          (let* ((now (context-navigator-project--now))
                 (elapsed (and (numberp context-navigator-project--last-switch-time)
                               (- now context-navigator-project--last-switch-time)))
                 (interval (if (and (boundp 'context-navigator-context-switch-interval)
                                    (numberp context-navigator-context-switch-interval))
                               context-navigator-context-switch-interval
                             0.0)))
            (if (or (not (numberp elapsed)) (>= elapsed interval))
                (progn
                  (setq context-navigator-project--last-root root
                        context-navigator-project--last-switch-time now)
                  (context-navigator-events-publish :project-switch root)
                  root)
              ;; Too soon: debounce a publish for the remaining time.
              (let ((delay (max 0.0 (- interval (or elapsed 0.0)))))
                (context-navigator-events-debounce
                 :project-switch delay
                 (lambda ()
                   (setq context-navigator-project--last-root root
                         context-navigator-project--last-switch-time (context-navigator-project--now))
                   (context-navigator-events-publish :project-switch root)))))
            nil))))))

(defun context-navigator-project--on-buffer-list-update ()
  "Hook: evaluate current or nearest interesting buffer for project switch.

Special handling: if the selected window is the Context Navigator (sidebar/buffer),
pick the first interesting window (file-visiting or allowed non-file modes) on the
current frame and switch based on it. This keeps auto-project responsive while the
sidebar is focused.

Ignore events coming from child frames (posframe/popups) and
when the current buffer is a minibuffer or a corfu internal buffer."
  ;; Defensive: ensure tracking vars exist even under mixed/old .elc loads
  (context-navigator-project--ensure-vars)
  (unless (or (context-navigator-project--child-frame-p)
              (minibufferp (current-buffer))
              (context-navigator-project--corfu-buffer-p (current-buffer)))
    (let* ((win (selected-window))
           (buf (window-buffer win))
           (nav-p (or (eq (window-parameter win 'context-navigator-view) 'sidebar)
                      (with-current-buffer buf
                        (eq major-mode 'context-navigator-view-mode))
                      (equal (buffer-name buf) "*context-navigator*"))))
      (if nav-p
          ;; Focus is in Navigator → use nearest interesting buffer on this frame
          (let* ((w (cl-find-if
                     (lambda (w)
                       (context-navigator-project--interesting-buffer-p (window-buffer w)))
                     (window-list (selected-frame) 'no-minibuffer)))
                 (target-buf (and w (window-buffer w))))
            (when target-buf
              (context-navigator-project--maybe-publish-switch target-buf)))
        ;; Normal path: use current buffer
        (context-navigator-project--maybe-publish-switch (current-buffer))))))

(defun context-navigator-project--on-window-selection-change (_frame)
  "Hook: react to real window selection changes only.

Special handling: if the selected window is the Context Navigator (sidebar/buffer),
switch based on the first interesting window (file or allowed non-file modes) on
the current frame so auto-project continues to follow real work buffers even while
the sidebar is focused.

Skip child frames (e.g., posframe popups), minibuffer windows and corfu buffers."
  ;; Defensive: ensure tracking vars exist even under mixed/old .elc loads
  (context-navigator-project--ensure-vars)
  (let ((win (selected-window)))
    (unless (or (context-navigator-project--child-frame-p (selected-frame))
                (minibufferp (window-buffer win))
                (context-navigator-project--corfu-buffer-p (window-buffer win)))
      (let* ((buf (window-buffer win))
             (nav-p (or (eq (window-parameter win 'context-navigator-view) 'sidebar)
                        (with-current-buffer buf
                          (eq major-mode 'context-navigator-view-mode))
                        (equal (buffer-name buf) "*context-navigator*"))))
        (if nav-p
            (let* ((w (cl-find-if
                       (lambda (w)
                         (context-navigator-project--interesting-buffer-p (window-buffer w)))
                       (window-list (selected-frame) 'no-minibuffer)))
                   (target-buf (and w (window-buffer w))))
              (when target-buf
                (context-navigator-project--maybe-publish-switch target-buf)))
          (context-navigator-project--maybe-publish-switch (window-buffer win)))))))

(defun context-navigator-project-setup-hooks ()
  "Install lightweight hooks to track project changes."
  ;; Defensive: ensure tracking vars are present before we toggle the flag
  (context-navigator-project--ensure-vars)
  (unless context-navigator-project--hooks-installed
    (add-hook 'buffer-list-update-hook #'context-navigator-project--on-buffer-list-update)
    (add-hook 'window-selection-change-functions #'context-navigator-project--on-window-selection-change)
    (setq context-navigator-project--hooks-installed t))
  t)

(defun context-navigator-project-teardown-hooks ()
  "Remove previously installed hooks."
  (when context-navigator-project--hooks-installed
    (remove-hook 'buffer-list-update-hook #'context-navigator-project--on-buffer-list-update)
    (remove-hook 'window-selection-change-functions #'context-navigator-project--on-window-selection-change)
    (setq context-navigator-project--hooks-installed nil))
  t)

;; --- Autoproj: Dired + GPTel/org gptel-aibo integration (built-in) ---

;; Helpers

(defun context-navigator-project--local-writable-dir-p (dir)
  "Return non-nil when DIR is a local, existing, writable directory."
  (and (stringp dir)
       (not (file-remote-p dir))
       (file-directory-p dir)
       (file-writable-p dir)))

(defun context-navigator-project--project-root-of-dir (dir)
  "Best-effort project root of DIR using project.el then projectile, or nil."
  (let (root)
    (when (and (fboundp 'project-current) (fboundp 'project-roots))
      (let ((default-directory dir))
        (setq root
              (ignore-errors
                (let ((pr (project-current nil))) ;; do not prompt when no project
                  (when pr
                    (car (project-roots pr))))))))
    (unless root
      (when (fboundp 'projectile-project-root)
        (let ((default-directory dir))
          (setq root (ignore-errors (projectile-project-root))))))
    (when (and root (not (file-remote-p root)))
      (directory-file-name root))))

(defun context-navigator-project--project-root-of-buffer (&optional buffer)
  "Best-effort project root of BUFFER's default-directory."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dir default-directory)
           (root (and (stringp dir)
                      (context-navigator-project--project-root-of-dir dir))))
      (and root (directory-file-name root)))))

(defun context-navigator-project--frame-file-project-root ()
  "Find the first file-visiting window on current frame; return its project root or nil.

Only returns local, writable roots."
  (let ((wins (window-list (selected-frame) 'no-minibuffer)))
    (cl-loop for w in wins
             for b = (window-buffer w)
             for f = (with-current-buffer b buffer-file-name)
             when (and f (file-exists-p f))
             do (let* ((dir (file-name-directory f))
                       (root (context-navigator-project--project-root-of-dir dir)))
                  (when (context-navigator-project--local-writable-dir-p root)
                    (cl-return (directory-file-name root))))
             finally (cl-return nil))))

;; Advice: extend interesting-buffer predicate for Dired and gptel-aibo minor mode
(when (fboundp 'context-navigator-project--interesting-buffer-p)
  (defun context-navigator-project--interesting-buffer-p--advice (orig buf)
    (or
     (funcall orig buf)
     (with-current-buffer buf
       (or
        ;; All Dired variants
        (derived-mode-p 'dired-mode)
        ;; org buffer with custom GPTel minor mode
        (bound-and-true-p gptel-aibo-mode)))))
  ;; Avoid duplicate advice on reload
  (unless (advice-member-p #'context-navigator-project--interesting-buffer-p--advice
                           'context-navigator-project--interesting-buffer-p)
    (advice-add 'context-navigator-project--interesting-buffer-p
                :around #'context-navigator-project--interesting-buffer-p--advice)))

;; Advice: add heuristic fallback for current-root resolution (policy B)
(when (fboundp 'context-navigator-project-root)
  (defun context-navigator-project--current-root--advice (orig &rest args)
    (let* ((root (ignore-errors (apply orig args))))
      (cond
       ;; Use original only when it is a local, writable directory
       ((and (stringp root)
             (context-navigator-project--local-writable-dir-p root))
        (directory-file-name (expand-file-name root)))
       (t
        ;; Heuristic: use project root from any file-visiting window in current frame
        (let ((fr (context-navigator-project--frame-file-project-root)))
          (and (stringp fr)
               (context-navigator-project--local-writable-dir-p fr)
               (directory-file-name fr)))))))
  ;; Avoid duplicate advice on reload
  (unless (advice-member-p #'context-navigator-project--current-root--advice
                           'context-navigator-project-root)
    (advice-add 'context-navigator-project-root
                :around #'context-navigator-project--current-root--advice)))

;; Removed duplicate Autoproj block (cn-autoproj--) to avoid double advices.

(provide 'context-navigator-project)
;;; context-navigator-project.el ends here
