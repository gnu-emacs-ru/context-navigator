;;; context-navigator-view-title.el --- Title builders and placement helpers -*- lexical-binding: t; -*-

;; Title string builders (project/group) and simple placement helpers.

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-core)
(require 'context-navigator-i18n)
(require 'context-navigator-icons)

(defgroup context-navigator-title nil
  "Title (project/group) for Navigator."
  :group 'context-navigator)

(defcustom context-navigator-title-enable nil
  "Deprecated: no-op. Title is shown in the header-line by default."
  :type 'boolean :group 'context-navigator-title)
(put 'context-navigator-title-enable 'obsolete-variable
     "Title is shown in the header-line by default.")

(defface context-navigator-title-face
  '((t :inherit default))
  "Face for pinned title text."
  :group 'context-navigator-title)

(defface context-navigator-headerline
  '((t :inherit default))
  "Face for Navigator header-line (applied via face-remap in the sidebar)."
  :group 'context-navigator-title)

(defvar-local context-navigator--pintitle-on nil)

;; Cached title (header-line) to avoid heavy recompute on every redisplay
(defvar-local context-navigator-title--cache-key nil)
(defvar-local context-navigator-title--cache-val nil)

;; Theme serial (increments on theme change to invalidate icon styling)
(defvar context-navigator-title--theme-serial 0)
(when (boundp 'after-enable-theme-functions)
  (add-hook 'after-enable-theme-functions
            (lambda (&rest _)
              (cl-incf context-navigator-title--theme-serial))))

(defun context-navigator-title-clear-cache ()
  "Clear cached header title for current buffer."
  (interactive)
  (setq context-navigator-title--cache-key nil
        context-navigator-title--cache-val nil))


(defcustom context-navigator-title-left-padding 2
  "Number of space characters to prefix to the pinned title text."
  :type 'integer :group 'context-navigator-title)


(defun context-navigator-title--mode ()
  (and (boundp 'context-navigator-view--mode) context-navigator-view--mode))

;; Internal helpers to simplify context-navigator-title--compute

(defun context-navigator-title--gather-state ()
  "Collect cheap state needed to render the title."
  (let* ((st   (ignore-errors (context-navigator--state-get)))
         (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
         (slug (and st (ignore-errors (context-navigator-state-current-group-slug st))))
         (mode (context-navigator-title--mode))
         (total   (or (and st (context-navigator-state-total-count st)) 0))
         (enabled (or (and st (context-navigator-state-enabled-count st)) 0)))
    (list :st st :root root :slug slug :mode mode :total total :enabled enabled)))

(defun context-navigator-title--make-cache-key (state)
  "Compose a cache key from STATE and the current theme serial."
  (list (plist-get state :mode)
        (plist-get state :root)
        (plist-get state :slug)
        (plist-get state :total)
        (plist-get state :enabled)
        context-navigator-title--theme-serial))

(defun context-navigator-title--indicator-state (total enabled)
  "Derive indicator state symbol from TOTAL and ENABLED counts."
  (cond
   ((<= total 0) 'absent)
   ((= enabled 0) 'absent)
   ((= enabled total) 'ok)
   (t 'mismatch)))

(defun context-navigator-title--icons (ind-state)
  "Build icons for project, group, arrow and indicator given IND-STATE."
  (let* ((icon-p (fboundp 'all-the-icons-material))
         (ico-proj (if icon-p
                       (ignore-errors
                         (propertize (all-the-icons-material "layers")
                                     'face '(:inherit context-navigator-title-face :foreground "DodgerBlue3" :height 0.9)
                                     'display '(raise -0.1)))
                     "📁"))
         (ico-gr (if icon-p
                     (ignore-errors
                       (propertize (all-the-icons-material "folder")
                                   'face '(:inherit context-navigator-title-face :foreground "MediumOrchid3" :height 0.9)
                                   'display '(raise -0.1)))
                   "🏷"))
         (arrow (if icon-p
                    (ignore-errors
                      (propertize (all-the-icons-material "arrow_forward")
                                  'face '(:inherit context-navigator-title-face :foreground "gray50" :height 0.83)
                                  'display '(raise -0.13)))
                  " ➔ "))
         (ico-ind
          (let ((s (ignore-errors
                     (when (fboundp 'context-navigator-icons-for-indicator)
                       (context-navigator-icons-for-indicator ind-state)))))
            (if (and (stringp s) (> (length s) 0))
                s
              (let* ((raw (pcase ind-state ('ok "●") ('mismatch "◐") (_ "○")))
                     (color (pcase ind-state ('ok "green4") ('mismatch "goldenrod2") (_ "gray"))))
                (propertize raw
                            'face (list :inherit 'context-navigator-title-face :foreground color :height 0.75)
                            'display '(raise 0.08)))))))
    (list :proj ico-proj :group ico-gr :arrow arrow :indicator ico-ind)))

(defun context-navigator-title--format-base (mode proj slug icons)
  "Compose the unpadded base title string from MODE, PROJ, SLUG and ICONS."
  (let ((ico-ind (plist-get icons :indicator))
        (ico-proj (plist-get icons :proj))
        (ico-gr (plist-get icons :group))
        (arrow (plist-get icons :arrow)))
    (cond
     ((eq mode 'groups) (format "%s  %s %s" ico-ind ico-proj proj))
     (slug               (format "%s  %s %s  %s %s %s" ico-ind ico-proj proj arrow ico-gr slug))
     (t                  (format "%s  %s %s" ico-ind ico-proj proj)))))

(defun context-navigator-title--pad (s)
  "Left-pad S according to `context-navigator-title-left-padding'."
  (concat (make-string (max 0 context-navigator-title-left-padding) ?\s) s))

(defun context-navigator-title--decorate (s)
  "Apply common text properties and interactivity to S, returning a new string."
  (let ((str (copy-sequence s)))
    (add-text-properties
     0 (length str)
     (list 'font-lock-face 'context-navigator-title-face
           'context-navigator-title t
           'context-navigator-header t)
     str)
    (when (and (boundp 'context-navigator-view--title-line-keymap)
               (keymapp context-navigator-view--title-line-keymap))
      (add-text-properties
       0 (length str)
       (list 'mouse-face 'mode-line-highlight
             'help-echo (and (fboundp 'context-navigator-i18n)
                             (context-navigator-i18n :title-toggle-hint))
             'keymap context-navigator-view--title-line-keymap
             'local-map context-navigator-view--title-line-keymap)
       str))
    str))

(defun context-navigator-title--project-name (root)
  "Return a short project name derived from ROOT or a localized fallback."
  (if (and (stringp root) (not (string-empty-p root)))
      (file-name-nondirectory (directory-file-name root))
    (context-navigator-i18n :global-context)))

(defun context-navigator-title--build (state)
  "Build and return the fully propertized title string for STATE (no caching).
This function composes:
- project name
- group slug (when in items mode and present)
- indicator/icon set
- padding and face properties"
  (let* ((mode    (plist-get state :mode))
         (root    (plist-get state :root))
         (slug    (plist-get state :slug))
         (total   (plist-get state :total))
         (enabled (plist-get state :enabled))
         (proj    (context-navigator-title--project-name root))
         (ind     (context-navigator-title--indicator-state total enabled))
         (icons   (context-navigator-title--icons ind))
         (base    (context-navigator-title--format-base mode proj slug icons))
         (txt     (context-navigator-title--pad base)))
    (context-navigator-title--decorate txt)))

(defun context-navigator-title--compute ()
  "Return a propertized title string with icons: [project[: group]] (cached)."
  (let* ((state (context-navigator-title--gather-state))
         ;; Keep cache key stable and cheap
         (cache-key (context-navigator-title--make-cache-key state)))
    ;; Fast path: reuse cached title when nothing relevant changed
    (when (equal cache-key context-navigator-title--cache-key)
      (cl-return-from context-navigator-title--compute context-navigator-title--cache-val))
    ;; Build fresh title and store in cache
    (let* ((s (context-navigator-title--build state)))
      (ignore-errors
        (when (fboundp 'context-navigator-debug)
          (context-navigator-debug :trace :ui
                                   "pinned-title cached: mode=%s slug=%s total=%s enabled=%s"
                                   (plist-get state :mode)
                                   (plist-get state :slug)
                                   (plist-get state :total)
                                   (plist-get state :enabled))))
      (setq context-navigator-title--cache-key cache-key
            context-navigator-title--cache-val s)
      s)))

(defun context-navigator-title--nav-window ()
  (catch 'hit
    (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                    (get-buffer context-navigator-view--buffer-name))))
      (dolist (w (window-list nil 'no-mini))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf))
          (throw 'hit w))))
    nil))

(defun context-navigator-title-enable ()
  "Enable inline pinned title (no posframe)."
  (setq context-navigator--pintitle-on t))


(defun context-navigator-title-refresh ()
  "No-op: inline title renders together with the buffer."
  nil)

(defun context-navigator-title-disable ()
  "Disable inline pinned title."
  (setq context-navigator--pintitle-on nil))

(defun context-navigator-title-fallback-line (&optional _mode)
  "Return inline title (posframe removed)."
  (context-navigator-title--compute))

(provide 'context-navigator-view-title)
;;; context-navigator-view-title.el ends here
