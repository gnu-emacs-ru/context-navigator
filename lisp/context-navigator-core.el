;;; context-navigator-core.el --- Core state and commands -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core: global state container (immutable updates), user-facing commands,
;; customization options. Minimal side-effects: module wiring (events/hooks).
;;
;; State is kept as a struct; all public mutators are pure (return new struct).
;; Only a thin setter installs new state in the global var.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-model)
(require 'context-navigator-events)
(require 'context-navigator-persist)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-project)
(require 'context-navigator-log)
(require 'context-navigator-compat)
(require 'context-navigator-groups)
(require 'context-navigator-context-blocks)

;; Ensure project tracking vars are always bound early (used by hooks during initialization)
(defvar context-navigator-project--last-root nil)
(defvar context-navigator-project--last-switch-time 0.0)

;; Ensure event bus vars are bound early to avoid void-variable during partial loads
(defvar context-navigator--event-subscribers (make-hash-table :test 'eq))
(defvar context-navigator--debounce-timers nil)

;; Forward declaration to avoid load cycle with sidebar/buffer view
(declare-function context-navigator-view-open "context-navigator-view" ())
(declare-function context-navigator-view-close "context-navigator-view" ())
(declare-function context-navigator-view-toggle "context-navigator-view" ())
(declare-function context-navigator-view-show-groups "context-navigator-view" ())
(declare-function context-navigator-buffer-open "context-navigator-view-buffer" ())
(declare-function context-navigator-buffer-close "context-navigator-view-buffer" ())
(declare-function context-navigator-buffer-toggle "context-navigator-view-buffer" ())
;; Forward declarations from project module (for byte-compiler friendliness)
(declare-function context-navigator-project-root "context-navigator-project" (&optional buffer))
(declare-function context-navigator-project--interesting-buffer-p "context-navigator-project" (buffer))
(declare-function context-navigator-project--frame-file-project-root "context-navigator-project" ())

(defgroup context-navigator nil
  "Modern context manager for Emacs/gptel (functional core)."
  :group 'convenience
  :prefix "context-navigator-")

(defcustom context-navigator-auto-refresh t
  "Auto refresh sidebar/model after external changes."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-view-width 45
  "Sidebar window width in columns."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-max-filename-length 64
  "Maximum display length for file names."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-context-switch-interval 0.7
  "Throttle interval (seconds) for project context switching."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-context-load-batch-size 64
  "Batch size for async context loading."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-gptel-apply-batch-size 20
  "How many items to push to gptel per tick when applying in the background."
  :type 'integer :group 'context-navigator)

(defcustom context-navigator-gptel-apply-batch-interval 0.05
  "Delay between gptel apply batches (seconds)."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-gptel-require-visible-window nil
  "When non-nil, defer applying to gptel until a gptel window is visible on the current frame."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-gptel-visible-poll-interval 0.5
  "Polling interval (seconds) to check for a visible gptel window when deferring apply."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-diagnostics-log-refresh-caller nil
  "When non-nil, log a brief backtrace when refresh bursts occur."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-autosave t
  "Autosave context file on model refresh (when not inhibited)."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-autosave-debounce 0.5
  "Debounce interval (seconds) for autosave on model refresh.

When many :model-refreshed events occur in quick succession, autosave is
debounced to avoid excessive disk IO. The debounced callback will use the
latest state at execution time; setting this to 0 disables debouncing
(autosave will still occur but may be suppressed by other inhibit flags)."
  :type 'number :group 'context-navigator)

(defcustom context-navigator-autoload t
  "Autoload context when switching projects."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-default-push-to-gptel t
  "Default session state for pushing Navigator context to gptel."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-default-auto-project-switch t
  "Default session state for automatic project switching."
  :type 'boolean :group 'context-navigator)

(defvar context-navigator--push-to-gptel context-navigator-default-push-to-gptel
  "Session flag: when non-nil, Navigator pushes current context to gptel.")

(defvar context-navigator--auto-project-switch context-navigator-default-auto-project-switch
  "Session flag: when non-nil, Navigator reacts to project switch events.")

(defcustom context-navigator-dir-name ".context"
  "Directory name under project root to store context."
  :type 'string :group 'context-navigator)

(defcustom context-navigator-context-file-name "context.el"
  "Context file name inside the context directory."
  :type 'string :group 'context-navigator)

(defcustom context-navigator-global-dir (expand-file-name "~/.context")
  "Global directory for context when project is not detected."
  :type 'directory :group 'context-navigator)

(defcustom context-navigator-create-default-group-file t
  "When non-nil, ensure 'default' group file exists on first load if missing.
Creates <root>/.context/default.el (or ~/.context/default.el in global mode)."
  :type 'boolean :group 'context-navigator)

(defconst context-navigator-persist-version 3
  "Persist format version used by Context Navigator (v3).")





(defcustom context-navigator-display-mode 'sidebar
  "How to display Navigator: 'buffer (magit-like) or 'sidebar."
  :type '(choice (const buffer) (const sidebar))
  :group 'context-navigator)

(defcustom context-navigator-remember-display-mode t
  "When non-nil, remember last chosen display mode between sessions."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-buffer-placement 'reuse-other-window
  "Placement policy for magit-like buffer mode:
- reuse-other-window : show in other window when available, else split
- split-right        : split selected window to the right
- split-below        : split selected window below"
  :type '(choice (const reuse-other-window)
                 (const split-right)
                 (const split-below))
  :group 'context-navigator)

(defcustom context-navigator-buffer-split-size 0.5
  "Size for splitting in buffer mode.
When 0<value<1 treat as fraction of current window; otherwise columns/rows."
  :type 'number :group 'context-navigator)

(defun context-navigator--sidebar-visible-p ()
  "Return non-nil when a window marked as 'sidebar displays our buffer on the current frame."
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (catch 'hit
      (dolist (w (window-list nil nil))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf)
                   (eq (window-parameter w 'context-navigator-view) 'sidebar))
          (throw 'hit t)))
      nil)))

(defun context-navigator--buffer-mode-visible-p ()
  "Return non-nil when Navigator buffer window (marked 'buffer) is visible on current frame."
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (catch 'hit
      (dolist (w (window-list nil nil))
        (when (and (window-live-p w)
                   (eq (window-buffer w) buf)
                   (eq (window-parameter w 'context-navigator-view) 'buffer))
          (throw 'hit t)))
      nil)))

;;;###autoload
(defun context-navigator-open ()
  "Open Navigator in the current display mode."
  (interactive)
  ;; Ensure core mode is active so hooks/subscriptions/auto-project are installed
  (unless (bound-and-true-p context-navigator-mode)
    (ignore-errors (context-navigator-mode 1)))
  (pcase context-navigator-display-mode
    ('sidebar (progn
                (require 'context-navigator-view nil t)
                (ignore-errors (context-navigator-view-open))))
    ('buffer  (progn
                (require 'context-navigator-view-buffer nil t)
                (ignore-errors (context-navigator-buffer-open))))
    (_        (progn
                (require 'context-navigator-view-buffer nil t)
                (ignore-errors (context-navigator-buffer-open))))))

;;;###autoload
(defun context-navigator-close ()
  "Close Navigator in the current display mode."
  (interactive)
  (pcase context-navigator-display-mode
    ('sidebar (progn
                (require 'context-navigator-view nil t)
                (ignore-errors (context-navigator-view-close))))
    ('buffer  (progn
                (require 'context-navigator-view-buffer nil t)
                (ignore-errors (context-navigator-buffer-close))))
    (_        (progn
                (require 'context-navigator-view-buffer nil t)
                (ignore-errors (context-navigator-buffer-close))))))

;;;###autoload
(defun context-navigator-toggle ()
  "Toggle Navigator visibility in the current display mode."
  (interactive)
  ;; Ensure core mode is active before toggling UI to avoid empty sidebar on first run
  (unless (bound-and-true-p context-navigator-mode)
    (ignore-errors (context-navigator-mode 1)))
  (pcase context-navigator-display-mode
    ('sidebar (progn
                (require 'context-navigator-view nil t)
                (ignore-errors (context-navigator-view-toggle))))
    ('buffer  (progn
                (require 'context-navigator-view-buffer nil t)
                (ignore-errors (context-navigator-buffer-toggle))))
    (_        (progn
                (require 'context-navigator-view-buffer nil t)
                (ignore-errors (context-navigator-buffer-toggle))))))

;;;###autoload
(defun context-navigator-display-mode-toggle ()
  "Toggle Navigator display mode between 'buffer and 'sidebar and reopen."
  (interactive)
  (setq context-navigator-display-mode
        (if (eq context-navigator-display-mode 'buffer) 'sidebar 'buffer))
  (when (and (boundp 'context-navigator-remember-display-mode)
             context-navigator-remember-display-mode)
    (ignore-errors
      (customize-save-variable 'context-navigator-display-mode context-navigator-display-mode)))
  (ignore-errors (context-navigator-close))
  (ignore-errors (context-navigator-open))
  (context-navigator-ui-info :display-mode-changed context-navigator-display-mode))

(cl-defstruct (context-navigator-state
               (:constructor context-navigator--state-make))
  "Global state (pure value).
Do not mutate fields in place; use helpers to return a new struct."
  (items nil :documentation "List<context-navigator-item>")
  (index (make-hash-table :test 'equal) :documentation "Key->item")
  (generation 0 :documentation "Monotonic generation number.")
  (inhibit-refresh nil)
  (inhibit-autosave nil)
  (loading-p nil)
  (last-project-root nil)
  (current-group-slug nil)
  (load-token 0))

(defvar context-navigator--state
  (context-navigator--state-make)
  "Global state value. Treat as immutable; use setters that return new values.")

(defun context-navigator--state-copy (state)
  "Return a shallow copy of STATE as a new context-navigator-state struct.
This avoids depending on cl-copy-struct and keeps copying explicit."
  (context-navigator--state-make
   :items (context-navigator-state-items state)
   :index (context-navigator-state-index state)
   :generation (context-navigator-state-generation state)
   :inhibit-refresh (context-navigator-state-inhibit-refresh state)
   :inhibit-autosave (context-navigator-state-inhibit-autosave state)
   :loading-p (context-navigator-state-loading-p state)
   :last-project-root (context-navigator-state-last-project-root state)
   :current-group-slug (context-navigator-state-current-group-slug state)
   :load-token (context-navigator-state-load-token state)))

;; Backwards-compatible alias used in some call sites/tests.
(defalias 'copy-context-navigator-state #'context-navigator--state-copy)

;;; Undo/Redo history (global, per-group) ------------------------------------

(defcustom context-navigator-undo-depth 10
  "Max history depth per group for Undo/Redo."
  :type 'integer :group 'context-navigator)

;; Internal history: group-key -> plist (:past list<snapshot> :future list<snapshot>)
(defvar context-navigator--history (make-hash-table :test 'equal))

(defun context-navigator--history-group-key ()
  "Return stable group key as ROOT|SLUG (or ~|<none>)."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st))))
    (format "%s|%s" (or root "~") (or slug "<none>"))))

(defun context-navigator--history-snapshot ()
  "Build a shallow copy snapshot of current items for history."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (context-navigator-state-items st))))
    (mapcar (lambda (it)
              (context-navigator-item-create
               :type (context-navigator-item-type it)
               :name (context-navigator-item-name it)
               :path (context-navigator-item-path it)
               :buffer (context-navigator-item-buffer it)
               :beg (context-navigator-item-beg it)
               :end (context-navigator-item-end it)
               :size (context-navigator-item-size it)
               :enabled (context-navigator-item-enabled it)
               :meta (context-navigator-item-meta it)))
            (or items '()))))

(defun context-navigator--history-get (key)
  "Return history cell for KEY, creating it if missing."
  (or (gethash key context-navigator--history)
      (let ((cell (list :past '() :future '())))
        (puthash key cell context-navigator--history)
        cell)))

(defun context-navigator--history-push (key snapshot)
  "Push SNAPSHOT to history for KEY; trim past and clear future."
  (let* ((cell (context-navigator--history-get key))
         (past (plist-get cell :past)))
    (setq past (cons snapshot past))
    (when (> (length past) (max 1 context-navigator-undo-depth))
      (setq past (cl-subseq past 0 context-navigator-undo-depth)))
    (setf (plist-get cell :past) past)
    (setf (plist-get cell :future) '())
    (puthash key cell context-navigator--history)))

(defun context-navigator-snapshot-push ()
  "Push current items snapshot into global history for this group."
  (interactive)
  (let* ((key (context-navigator--history-group-key))
         (snap (context-navigator--history-snapshot)))
    (context-navigator--history-push key snap)
    key))

(defun context-navigator--apply-snapshot (snapshot)
  "Apply SNAPSHOT to the model and push to gptel when auto-push is ON."
  (ignore-errors (context-navigator-set-items (or snapshot '())))
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel)
    (ignore-errors (context-navigator-gptel-apply snapshot))))

(defun context-navigator--history-undo (key)
  "Return previous snapshot and move current to future; nil when empty."
  (let* ((cell (context-navigator--history-get key))
         (past (plist-get cell :past)))
    (when (consp past)
      (let* ((prev (car past))
             (rest (cdr past))
             (cur (context-navigator--history-snapshot))
             (future (cons cur (plist-get cell :future))))
        (setf (plist-get cell :past) rest)
        (setf (plist-get cell :future) future)
        (puthash key cell context-navigator--history)
        prev))))

(defun context-navigator--history-redo (key)
  "Return next snapshot from future and push current to past; nil when empty."
  (let* ((cell (context-navigator--history-get key))
         (future (plist-get cell :future)))
    (when (consp future)
      (let* ((next (car future))
             (rest (cdr future))
             (cur (context-navigator--history-snapshot))
             (past (cons cur (plist-get cell :past))))
        (setf (plist-get cell :future) rest)
        (setf (plist-get cell :past) past)
        (puthash key cell context-navigator--history)
        next))))

;;;###autoload
(defun context-navigator-undo ()
  "Undo last change for the current group's model items."
  (interactive)
  (let* ((key (context-navigator--history-group-key))
         (prev (context-navigator--history-undo key)))
    (if prev
        (progn
          (context-navigator--apply-snapshot prev)
          ;; Форсируем перерисовку: инвалидация кеша контента (plain-text хеш)
          ;; нужна для случаев, когда изменились только фейсы/проперти.
          (let ((buf (get-buffer "*context-navigator*")))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local context-navigator-render--last-hash nil))))
          (when (fboundp 'context-navigator-view--render-if-visible)
            (ignore-errors (context-navigator-view--render-if-visible)))
          (context-navigator-ui-info :razor-undo))
      (context-navigator-ui-info :nothing-to-undo))))

;;;###autoload
(defun context-navigator-redo ()
  "Redo previously undone change for the current group's model items."
  (interactive)
  (let* ((key (context-navigator--history-group-key))
         (next (context-navigator--history-redo key)))
    (if next
        (progn
          (context-navigator--apply-snapshot next)
          ;; Принудительная перерисовка (как в контролах): инвалидация кеша + render now
          (let ((buf (get-buffer "*context-navigator*")))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local context-navigator-render--last-hash nil))))
          (when (fboundp 'context-navigator-view--render-if-visible)
            (ignore-errors (context-navigator-view--render-if-visible)))
          (context-navigator-ui-info :razor-redo))
      (context-navigator-ui-info :nothing-to-redo))))

(defun context-navigator--log (fmt &rest args)
  "Log via centralized logger at :info level under :core topic."
  (apply #'context-navigator-debug (append (list :info :core fmt) args)))

(defun context-navigator--state-get ()
  "Return current global state value."
  context-navigator--state)

(defun context-navigator--set-state (new-state)
  "Install NEW-STATE as the current global state."
  (setq context-navigator--state new-state))

(defun context-navigator--state-with-items (state items)
  "Return new STATE' with ITEMS and recomputed index/generation."
  ;; Keep original order and allow duplicates in ITEMS; index will reflect last-wins semantics.
  (let* ((uni items)
         (idx (context-navigator-model-build-index uni))
         (new (context-navigator--state-copy state)))
    (setf (context-navigator-state-items new) uni)
    (setf (context-navigator-state-index new) idx)
    (setf (context-navigator-state-generation new)
          (1+ (context-navigator-state-generation new)))
    new))

;;;###autoload
(defun context-navigator-refresh ()
  "Recompute indices and publish a light refresh event."
  (interactive)
  ;; Optional diagnostics: when `context-navigator-diagnostics-log-refresh-caller' is non-nil,
  ;; log a trimmed backtrace on refresh bursts (>=10 calls/sec).
  (defvar context-navigator--refresh-log-ts 0.0)
  (defvar context-navigator--refresh-log-count 0)
  (when (and (boundp 'context-navigator-diagnostics-log-refresh-caller)
             context-navigator-diagnostics-log-refresh-caller)
    (let* ((now (float-time))
           (prev context-navigator--refresh-log-ts)
           (cnt context-navigator--refresh-log-count))
      (if (< (- now prev) 1.0)
          (setq context-navigator--refresh-log-count (1+ cnt))
        (setq context-navigator--refresh-log-ts now
              context-navigator--refresh-log-count 1))
      (when (>= context-navigator--refresh-log-count 10)
        (setq context-navigator--refresh-log-ts now
              context-navigator--refresh-log-count 0)
        (let ((bt (ignore-errors (with-output-to-string (backtrace)))))
          (when (and (stringp bt) (fboundp 'context-navigator-debug))
            (ignore-errors
              (context-navigator-debug :trace :core "refresh burst — caller backtrace:\n%s"
                                       (mapconcat #'identity
                                                  (cl-subseq (split-string bt "\n") 0
                                                             (min 22 (length (split-string bt "\n"))))
                                                  "\n"))))))))
  (let* ((cur (context-navigator--state-get))
         (items (context-navigator-state-items cur))
         (new (context-navigator--state-with-items
               (context-navigator--state-copy cur) items)))
    (context-navigator--set-state new)
    (context-navigator-events-publish :model-refreshed new)
    (context-navigator--log "Refreshed generation=%s"
                            (context-navigator-state-generation new))
    new))

(defcustom context-navigator-global-key "C-c n"
  "Global key sequence for opening the Context Navigator transient.
Default is \"C-c n\"."
  :type '(choice (const :tag "None" nil) (string :tag "Key sequence"))
  :group 'context-navigator
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'context-navigator--update-global-keybinding)
           (ignore-errors (context-navigator--update-global-keybinding)))))

(defvar context-navigator--current-global-key nil
  "Internally tracks the currently active global keybinding for the transient.")

(defun context-navigator--update-global-keybinding ()
  "Apply `context-navigator-global-key' globally and in the mode map."
  (let ((old context-navigator--current-global-key)
        (new (and (stringp context-navigator-global-key)
                  (not (string-empty-p context-navigator-global-key))
                  context-navigator-global-key)))
    ;; Remove old binding from both global-map and mode-map
    (when old
      (ignore-errors (define-key global-map (kbd old) nil))
      (when (keymapp context-navigator-mode-map)
        (ignore-errors (define-key context-navigator-mode-map (kbd old) nil))))
    ;; Install new binding in global-map and in mode-map
    (when new
      (ignore-errors (define-key global-map (kbd new) #'context-navigator-view-open-menu))
      (when (keymapp context-navigator-mode-map)
        (ignore-errors (define-key context-navigator-mode-map (kbd new) #'context-navigator-view-open-menu))))
    (setq context-navigator--current-global-key new)))

(defvar context-navigator-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Remap delete-other-windows to close navigator side windows first when mode enabled.
    (define-key m [remap delete-other-windows] #'context-navigator-delete-other-windows)
    ;; no default global bindings; see `context-navigator-global-key'
    m)
  "Keymap for `context-navigator-mode'.")

(defun context-navigator-delete-other-windows ()
  "Close any Context Navigator sidebar windows first; then call `delete-other-windows'.
This avoids the situation where a side window (the sidebar) becomes the only
window after `delete-other-windows' and preserves the user's intended layout.

If no sidebar windows are present, behave like `delete-other-windows'."
  (interactive)
  (let ((buf (and (boundp 'context-navigator-view--buffer-name)
                  (get-buffer context-navigator-view--buffer-name))))
    (when buf
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (delete-window w)))))
  ;; Finally, perform the normal delete-other-windows in the currently selected frame.
  (when (fboundp 'delete-other-windows)
    (call-interactively #'delete-other-windows)))

;; Apply user-defined binding (if any) on load
(ignore-errors (context-navigator--update-global-keybinding))

;; Keep keybinding in sync even when set via setq or let.
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'context-navigator-global-key
   (lambda (_sym _newval _op _where)
     (ignore-errors (context-navigator--update-global-keybinding)))))

(defvar context-navigator--event-tokens nil
  "Subscription tokens registered by core while the mode is enabled.")

;; gptel background apply scheduler (batched)
(defvar context-navigator--gptel-batch-timer nil)
(defvar context-navigator--gptel-batch-queue nil)
(defvar context-navigator--gptel-batch-total 0)
(defvar context-navigator--gptel-batch-token 0)
(defvar context-navigator--gptel-visible-poll-timer nil)

(defvar context-navigator--suppress-apply-until nil
  "If non-nil, a time (float-time) until which applying to gptel is suppressed.")

(defun context-navigator--apply-allowed-p ()
  "Return non-nil when it is allowed to apply to gptel now."
  (let ((till context-navigator--suppress-apply-until))
    (or (null till)
        (> (float-time) till))))

(defun context-navigator--suppress-apply-for (seconds)
  "Suppress applying to gptel for SECONDS (float)."
  (setq context-navigator--suppress-apply-until
        (+ (float-time) (or seconds 1.0))))

(defun context-navigator-toggle-push-to-gptel ()
  "Toggle session flag to push Navigator context to gptel."
  (interactive)
  (setq context-navigator--push-to-gptel (not context-navigator--push-to-gptel))
  ;; Reset one-shot notification flag when user manually enables push again,
  ;; so future threshold gates can notify once more.
  (when context-navigator--push-to-gptel
    (setq context-navigator--autopush-disabled-notified nil))
  (context-navigator-ui-info :push-state
                             (context-navigator-i18n (if context-navigator--push-to-gptel :on :off)))
  (context-navigator-refresh))

(defun context-navigator-toggle-auto-project-switch ()
  "Toggle session flag for automatic project switching.
When turning ON, ensure `context-navigator-mode' is enabled so the core subscribes
to :project-switch events.

Special case: if the command is invoked while the Navigator buffer/window is
selected, prefer the first buffer on the current frame (excluding the Navigator)
that belongs to a project and switch to that project. Otherwise, fall back to
the generic picker:
- the most recently visited file-backed buffer's project, or
- the first \"interesting\" buffer (file/gptel/Dired),
or global (nil) when nothing is found."
  (interactive)
  (setq context-navigator--auto-project-switch (not context-navigator--auto-project-switch))
  (context-navigator-ui-info :auto-project-state
                             (context-navigator-i18n (if context-navigator--auto-project-switch :on :off)))
  ;; Force a quick UI refresh for toggles
  (context-navigator-refresh)
  ;; On enabling auto-project, ensure mode is on and then publish a project switch.
  (when context-navigator--auto-project-switch
    (unless (bound-and-true-p context-navigator-mode)
      (context-navigator-mode 1))
    (let ((root
           (ignore-errors
             (let* ((in-nav
                     (or (eq major-mode 'context-navigator-view-mode)
                         (let ((w (selected-window)))
                           (and (window-live-p w)
                                (memq (window-parameter w 'context-navigator-view) '(sidebar buffer))))))
                    (pick-frame-first
                     (lambda ()
                       (catch 'found
                         (dolist (w (window-list (selected-frame) 'no-minibuffer))
                           (when (window-live-p w)
                             (let* ((buf (window-buffer w))
                                    (is-nav
                                     (or (eq buf (and (boundp 'context-navigator-view--buffer-name)
                                                      (get-buffer context-navigator-view--buffer-name)))
                                         (with-current-buffer buf
                                           (eq major-mode 'context-navigator-view-mode)))))
                               (unless is-nav
                                 (let ((rr (ignore-errors (context-navigator-project-root buf))))
                                   (when (and (stringp rr) (not (string-empty-p rr)))
                                     (throw 'found rr)))))))
                         nil))))
               (or (and in-nav (funcall pick-frame-first))
                   (context-navigator--pick-root-for-autoproject))))))
      (context-navigator-events-publish :project-switch root))))

(defun context-navigator-push-to-gptel-now ()
  "Manually push current model to gptel asynchronously (batched)."
  (interactive)
  (if (not (context-navigator-gptel-available-p))
      (progn
        (context-navigator-ui-info :gptel-not-available)
        nil)
    ;; Best-effort clear first, then start batched apply to avoid UI hangs.
    (ignore-errors (context-navigator-gptel-clear-all-now))
    (let* ((st (context-navigator--state-get))
           (items (and st (context-navigator-state-items st)))
           (token (and st (context-navigator-state-load-token st))))
      (let ((context-navigator-gptel-require-visible-window nil))
        (ignore-errors (context-navigator--gptel-defer-or-start (or items '()) token))))))

(defun context-navigator-clear-gptel-now ()
  "Clear gptel context and disable all items in the current model."
  (interactive)
  ;; Clear gptel context first (best-effort)
  (ignore-errors
    (if (context-navigator-gptel-available-p)
        (context-navigator-gptel-clear-all-now)
      (context-navigator-gptel-apply '())))
  ;; Disable all items in the model so they are not re-pushed
  (let* ((st (context-navigator--state-get))
         (items (and st (context-navigator-state-items st))))
    (when (listp items)
      (let ((disabled
             (mapcar (lambda (it)
                       (context-navigator-item-create
                        :type (context-navigator-item-type it)
                        :name (context-navigator-item-name it)
                        :path (context-navigator-item-path it)
                        :buffer (context-navigator-item-buffer it)
                        :beg (context-navigator-item-beg it)
                        :end (context-navigator-item-end it)
                        :size (context-navigator-item-size it)
                        :enabled nil
                        :meta (context-navigator-item-meta it)))
                     items)))
        (context-navigator-set-items disabled))))
  ;; Notify listeners and report
  (ignore-errors (context-navigator-events-publish :gptel-change :cleared))
  (context-navigator-ui-info :gptel-cleared))

(defun context-navigator-switch-to-current-buffer-project ()
  "Switch Navigator to the project of the current buffer (manual)."
  (interactive)
  (let ((root (ignore-errors (context-navigator-project-root (current-buffer)))))
    (let ((context-navigator--auto-project-switch t))
      (context-navigator--on-project-switch root))))

(defun context-navigator--load-init-state (root slug)
  "Подготовить core-состояние к загрузке группы SLUG для ROOT.
Возвращает новый load TOKEN."
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur))
         (token (1+ (or (context-navigator-state-load-token new) 0))))
    ;; During load, suppress empty autosaves to avoid wiping files mid-reload.
    (setq context-navigator-persist-suppress-empty-save t)
    (setf (context-navigator-state-inhibit-refresh new) t)
    (setf (context-navigator-state-inhibit-autosave new) t)
    (setf (context-navigator-state-loading-p new) t)
    (setf (context-navigator-state-current-group-slug new) slug)
    (setf (context-navigator-state-last-project-root new) root)
    (setf (context-navigator-state-load-token new) token)
    (context-navigator--set-state new)
    (context-navigator-events-publish :context-load-start root)
    token))

(defun context-navigator--persist-current-async (root slug)
  "Асинхронно записать :current=SLUG в state.el для ROOT, если изменилось."
  (run-at-time
   0 nil
   (lambda ()
     (let* ((st (or (context-navigator-persist-state-load root) '()))
            (st1 (if (plist-member st :version) (copy-sequence st)
                   (plist-put (copy-sequence st) :version 1)))
            (cur (plist-get st1 :current)))
       (unless (equal cur slug)
         (setq st1 (plist-put st1 :current slug))
         (ignore-errors (context-navigator-persist-state-save root st1)))))))

(defun context-navigator--prepare-gptel-for-load ()
  "Отменить батчи gptel и, если автопуш включён, асинхронно очистить контекст."
  (context-navigator--gptel-cancel-batch)
  (when context-navigator--push-to-gptel
    (run-at-time 0 nil
                 (lambda ()
                   (ignore-errors (context-navigator-gptel-clear-all-now))))))

(defun context-navigator--finalize-load ()
  "Снять inhibit-флаги и сбросить флаг загрузки."
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur)))
    (setf (context-navigator-state-inhibit-refresh new) nil)
    (setf (context-navigator-state-inhibit-autosave new) nil)
    (setf (context-navigator-state-loading-p new) nil)
    (context-navigator--set-state new)))

(defun context-navigator--load-alive-p (token)
  "Return non-nil when there is an in-flight load matching TOKEN."
  (let ((st (context-navigator--state-get)))
    (and (context-navigator-state-p st)
         (context-navigator-state-loading-p st)
         (= (or (context-navigator-state-load-token st) 0) token))))

(defun context-navigator--push-after-load--single (items token)
  "Start or defer gptel apply for ITEMS (current group), bound to LOAD TOKEN."
  (run-at-time
   0 nil
   (lambda ()
     (let ((context-navigator-gptel-require-visible-window nil))
       (ignore-errors
         (context-navigator--gptel-defer-or-start (or items '()) token))))))

(defun context-navigator--push-after-load (root items token)
  "Push loaded ITEMS to gptel respecting multi-group selection for ROOT.
Does nothing when push→gptel is disabled."
  (when context-navigator--push-to-gptel
    (let* ((ps (and (stringp root)
                    (ignore-errors (context-navigator-persist-state-load root))))
           (mg (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
           (sel (and (listp ps) (plist-member ps :selected) (plist-get ps :selected))))
      (if (and mg (listp sel) (> (length sel) 0))
          ;; Multi-group ON: aggregate ALL items across the selection (ignore enabled)
          (context-navigator-collect-items-for-groups-async
           root sel
           (lambda (agg-items)
             (let ((forced (context-navigator--force-enable-items agg-items)))
               (context-navigator--push-after-load--single forced token))))
        ;; Single-group or no selection: push current group's items as-is
        (context-navigator--push-after-load--single items token)))))

(defun context-navigator--install-loaded-items (root slug items prev-items)
  "Install loaded ITEMS into the model; on failure keep PREV-ITEMS and log a warning."
  (if (and (listp items))
      (context-navigator-set-items items)
    (context-navigator-debug :warn :persist
                             "load failed for root=%s slug=%s; keeping previous items"
                             root slug)
    (context-navigator-set-items (or prev-items '()))))

(defun context-navigator--handle-group-loaded (root slug token items)
  "Обработать результат загрузки ITEMS для ROOT/SLUG с проверкой TOKEN.
Пуш в gptel выполняется, только если он включён. Всегда выполняет финализацию и события."
  (let* ((st (context-navigator--state-get))
         (prev-items (and st (context-navigator-state-items st)))
         (alive (context-navigator--load-alive-p token)))
    (when alive
      (context-navigator--push-after-load root items token)
      (context-navigator--install-loaded-items root slug items prev-items))
    ;; Финализация и события — безусловно.
    (context-navigator--finalize-load)
    ;; Lift the empty-save suppression after a group load completes.
    (setq context-navigator-persist-suppress-empty-save nil)
    (context-navigator-events-publish :context-load-done root (listp items))
    (context-navigator-events-publish :group-switch-done root slug (listp items))))

(defun context-navigator--load-group-for-root (root slug)
  "Загрузить группу SLUG для ROOT (или глобально) асинхронно и при необходимости применить к gptel."
  (let ((token (context-navigator--load-init-state root slug)))
    (context-navigator--persist-current-async root slug)
    (context-navigator--prepare-gptel-for-load)
    (context-navigator-events-publish :group-switch-start root slug)
    (context-navigator-persist-load-group-async
     root slug
     (lambda (items)
       (context-navigator--handle-group-loaded root slug token items)))))

(defun context-navigator--load-context-for-root (root)
  "Load current group for ROOT (or global) using state.el.

Behavior:
- If :current is missing in state.el, set it to \"default\"
- If :current points to a non-existent group file, rewrite it to \"default\"
- Then delegate actual loading to `context-navigator--load-group-for-root'."
  (let* ((st (or (context-navigator-persist-state-load root) '()))
         (slug (or (plist-get st :current) "default"))
         (st* (if (plist-member st :version) (copy-sequence st)
                (plist-put (copy-sequence st) :version 1))))
    ;; Ensure :current exists (save async)
    (unless (plist-member st* :current)
      (setq st* (plist-put st* :current slug))
      (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-persist-state-save root st*)))))
    ;; If the current group file is missing/unreadable, fall back to default.
    ;; Избегаем синхронного stat на TRAMP.
    (let* ((file (ignore-errors (context-navigator-persist-context-file root slug)))
           (remote (and (stringp file) (file-remote-p file)))
           (missing (or (null file) (and (not remote) (not (file-readable-p file))))))
      (when missing
        (setq slug "default")
        (setq st* (plist-put (copy-sequence st*) :current slug))
        (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-persist-state-save root st*))))))
    ;; Ensure default group file exists when selected
    (when (and (string= slug "default")
               (boundp 'context-navigator-create-default-group-file)
               context-navigator-create-default-group-file)
      (let ((f (ignore-errors (context-navigator-persist-context-file root slug))))
        (when (and (stringp f) (not (file-exists-p f)))
          (ignore-errors (context-navigator-persist-save '() root slug)))))
    (context-navigator--load-group-for-root root slug)))

(defun context-navigator--gptel-visible-p ()
  "Return non-nil if a gptel buffer is visible in any window on the selected frame."
  (catch 'yes
    (dolist (w (window-list nil 'no-mini))
      (when (window-live-p w)
        (with-current-buffer (window-buffer w)
          (when (derived-mode-p 'gptel-mode)
            (throw 'yes t)))))
    nil))

(defun context-navigator--gptel-cancel-batch ()
  "Cancel any pending batched apply to gptel."
  (when (timerp context-navigator--gptel-batch-timer)
    (cancel-timer context-navigator--gptel-batch-timer))
  (setq context-navigator--gptel-batch-timer nil
        context-navigator--gptel-batch-queue nil
        context-navigator--gptel-batch-total 0
        context-navigator--gptel-batch-token 0)
  (when (timerp context-navigator--gptel-visible-poll-timer)
    (cancel-timer context-navigator--gptel-visible-poll-timer))
  (setq context-navigator--gptel-visible-poll-timer nil)
  ;; Notify listeners that any pending batch was cancelled.
  (ignore-errors (context-navigator-events-publish :gptel-change :batch-cancel))
  t)

(defun context-navigator--gptel-start-batch (items token)
  "Start batched add of ITEMS to gptel, bound to LOAD TOKEN.
Assumes gptel has been cleared beforehand."
  (context-navigator--gptel-cancel-batch)
  (setq context-navigator--gptel-batch-queue (copy-sequence (or items '())))
  (setq context-navigator--gptel-batch-total (length context-navigator--gptel-batch-queue))
  (setq context-navigator--gptel-batch-token token)
  ;; If there is nothing to do, avoid publishing a start event or creating timers.
  ;; Publish a done event immediately so any UI that might have briefly started
  ;; a loader can clear itself reliably.
  (if (<= (or context-navigator--gptel-batch-total 0) 0)
      (ignore-errors (context-navigator-events-publish :gptel-change :batch-done context-navigator--gptel-batch-total))
    ;; Notify listeners that a batch is starting (provide total count).
    (ignore-errors (context-navigator-events-publish :gptel-change :batch-start context-navigator--gptel-batch-total))
    (let ((kick (lambda ()
                  (let* ((st (context-navigator--state-get)))
                    ;; Only cancel when we have a meaningful batch token and it mismatches the current state load-token.
                    ;; This avoids cancelling batches started with token 0/nil (manual operations) when load-token differs.
                    (when (and context-navigator--gptel-batch-token
                               (context-navigator-state-p st)
                               (not (= (or (context-navigator-state-load-token st) 0)
                                       context-navigator--gptel-batch-token)))
                      (context-navigator--gptel-cancel-batch))
                    (when context-navigator--gptel-batch-queue
                      (let ((n (max 1 (or context-navigator-gptel-apply-batch-size 20)))
                            (ops 0))
                        (dotimes (_i n)
                          (when-let ((it (car context-navigator--gptel-batch-queue)))
                            (setq context-navigator--gptel-batch-queue (cdr context-navigator--gptel-batch-queue))
                            (when (context-navigator-item-enabled it)
                              (ignore-errors (context-navigator-gptel-add-one it)))
                            (setq ops (1+ ops))))
                        (when (null context-navigator--gptel-batch-queue)
                          ;; done
                          (context-navigator-events-publish :gptel-change :batch-done context-navigator--gptel-batch-total)
                          (context-navigator--gptel-cancel-batch))))))))
      (setq context-navigator--gptel-batch-timer
            (run-at-time 0 (or context-navigator-gptel-apply-batch-interval 0.05) kick)))))

(defun context-navigator--gptel-defer-or-start (items token)
  "Defer batched apply until gptel window is visible when required.
Otherwise start immediately."
  (if (and context-navigator-gptel-require-visible-window
           (not (context-navigator--gptel-visible-p)))
      ;; defer and poll
      (progn
        (context-navigator--gptel-cancel-batch)
        (setq context-navigator--gptel-batch-queue (copy-sequence (or items '())))
        (setq context-navigator--gptel-batch-total (length context-navigator--gptel-batch-queue))
        (setq context-navigator--gptel-batch-token token)
        (let ((poll (lambda ()
                      (let ((st (context-navigator--state-get)))
                        (if (not (and (context-navigator-state-p st)
                                      (= (or (context-navigator-state-load-token st) 0)
                                         context-navigator--gptel-batch-token)))
                            (context-navigator--gptel-cancel-batch)
                          (when (context-navigator--gptel-visible-p)
                            (when (timerp context-navigator--gptel-visible-poll-timer)
                              (cancel-timer context-navigator--gptel-visible-poll-timer))
                            (setq context-navigator--gptel-visible-poll-timer nil)
                            (context-navigator--gptel-start-batch context-navigator--gptel-batch-queue token)))))))
          (setq context-navigator--gptel-visible-poll-timer
                (run-at-time 0 (or context-navigator-gptel-visible-poll-interval 0.5) poll))))
    ;; start now
    (context-navigator--gptel-start-batch items token)))

;; --- Multi-group helpers -----------------------------------------------------

(defcustom context-navigator-multigroup-autopush-threshold 100
  "Max number of enabled items for auto-push across selected groups.
When the aggregated enabled items exceed this threshold, auto-push is disabled
and only manual push is allowed."
  :type 'integer :group 'context-navigator)

(defvar context-navigator--autopush-disabled-notified nil
  "Non-nil when we've already shown the auto-push disabled notification for the current session/selection.")

(defun context-navigator--force-enable-items (items)
  "Return copies of ITEMS with :enabled t, preserving other fields."
  (mapcar (lambda (it)
            (context-navigator-item-create
             :type (context-navigator-item-type it)
             :name (context-navigator-item-name it)
             :path (context-navigator-item-path it)
             :buffer (context-navigator-item-buffer it)
             :beg (context-navigator-item-beg it)
             :end (context-navigator-item-end it)
             :size (context-navigator-item-size it)
             :enabled t
             :meta (context-navigator-item-meta it)))
          (or items '())))

(defun context-navigator--enabled-only (items)
  "Return a list of ITEMS filtered to enabled ones (pure)."
  (cl-remove-if-not (lambda (it) (context-navigator-item-enabled it)) (or items '())))

(defun context-navigator-collect-items-for-groups-async (root slugs callback &optional enabled-only)
  "Asynchronously collect items for SLUGS under ROOT and call CALLBACK with a deduplicated list.
When ENABLED-ONLY is non-nil, filter to enabled items before deduplication."
  (let* ((acc '())
         (pending (copy-sequence (or slugs '())))
         (dedup (lambda (lst) (context-navigator-model-uniq (or lst '()))))
         (filter-fn (if enabled-only #'context-navigator--enabled-only (lambda (x) x))))
    (cl-labels
        ((step ()
           (if (null pending)
               (let ((items (funcall dedup (nreverse acc))))
                 ;; Pair :context-load-start (published by persist loader) with a done event
                 ;; so the View clears its lightweight preloader.
                 (ignore-errors (context-navigator-events-publish :context-load-done root t))
                 (funcall callback items))
             (let* ((slug (car pending)))
               (setq pending (cdr pending))
               (context-navigator-persist-load-group-async
                root slug
                (lambda (items)
                  (let ((filtered (funcall filter-fn items)))
                    (setq acc (append filtered acc)))
                  (run-at-time 0 nil #'step)))))))
      (run-at-time 0 nil #'step))))

(defun context-navigator-collect-enabled-items-for-groups-async (root slugs callback)
  "Backward-compatible wrapper: collect only enabled items for SLUGS under ROOT."
  (context-navigator-collect-items-for-groups-async root slugs callback t))

(defun context-navigator-apply-groups-now (root slugs)
  "Manually push aggregated ALL items from SLUGS under ROOT to gptel (batched)."
  (interactive)
  (context-navigator-collect-items-for-groups-async
   root slugs
   (lambda (items)
     (let* ((all (or items '()))
            (n (length all)))
       (if (= n 0)
           (context-navigator-ui-info :no-items-in-selection)
         ;; Clear gptel (without touching model) and push forced-enabled copies
         (ignore-errors (context-navigator-gptel-clear-all-now))
         (let* ((st (context-navigator--state-get))
                (token (and st (context-navigator-state-load-token st)))
                (forced (context-navigator--force-enable-items all)))
           (ignore-errors (context-navigator--gptel-defer-or-start forced token)))
         (context-navigator-ui-info :pushed-items n))))))

;; ---- Multi-group auto-push gating (small helpers + thin effect wrapper) ----

(defun context-navigator--selected-group-slugs-for-root (root)
  "Pure: return list of selected group slugs for ROOT from state.el."
  (let* ((st (and (stringp root)
                  (ignore-errors (context-navigator-persist-state-load root)))))
    (let ((sel (and (listp st) (plist-member st :selected) (plist-get st :selected))))
      (if (listp sel) sel '()))))

(defun context-navigator--sum-enabled-for-groups (root slugs)
  "Pure: sum enabled counters across SLUGS by reading their v3 files."
  (let ((sum 0))
    (dolist (slug slugs sum)
      (let* ((file (ignore-errors (context-navigator-persist-context-file root slug)))
             (en.t (and (stringp file)
                        (ignore-errors (context-navigator-persist-group-enabled-count file))))
             (en (and (consp en.t) (car en.t))))
        (when (integerp en)
          (setq sum (+ sum en)))))))

(defun context-navigator-autopush-gate-on-selection ()
  "Side-effect: if auto-push is ON and selected groups exceed threshold, disable push flag.
Uses quick per-group enabled counters; avoids heavy IO and stays responsive.
Notifies user once per session when auto-push is disabled due to a large selection."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (sel (context-navigator--selected-group-slugs-for-root root))
         (thr (or context-navigator-multigroup-autopush-threshold 100)))
    (when (and (stringp root)
               (listp sel) (> (length sel) 0))
      (let ((sum (context-navigator--sum-enabled-for-groups root sel)))
        (cond
         ((> sum thr)
          ;; Disable push if it is currently enabled
          (when (and (boundp 'context-navigator--push-to-gptel)
                     context-navigator--push-to-gptel)
            (setq context-navigator--push-to-gptel nil))
          (context-navigator-ui-info :push-state (context-navigator-i18n :off))
          ;; Notify user once per session/occurrence
          (unless context-navigator--autopush-disabled-notified
            (context-navigator-ui-info :autopush-disabled-threshold sum thr)
            (setq context-navigator--autopush-disabled-notified t))
          (context-navigator-debug :info :core
                                   "auto-push disabled by gate (enabled=%s > thr=%s)" sum thr))
         ((<= sum thr)
          ;; Reset notification so future gates can notify again.
          (setq context-navigator--autopush-disabled-notified nil)))))))


(defun context-navigator--on-project-switch (root)
  "Handle :project-switch event with ROOT (string or nil)."
  (if (not context-navigator--auto-project-switch)
      ;; Автопереключение выключено — игнорируем событие и не трогаем state/заголовок.
      (context-navigator--log "Project switch (ignored, auto-project OFF) -> %s" (or root "~"))
    (progn
      (ignore-errors (context-navigator-events-cancel :autosave))
      (let* ((cur (context-navigator--state-get))
             (new (context-navigator--state-copy cur)))
        (setf (context-navigator-state-inhibit-refresh new) t)
        (setf (context-navigator-state-inhibit-autosave new) t)
        (setf (context-navigator-state-loading-p new) t)
        (setf (context-navigator-state-last-project-root new) root)
        (context-navigator--set-state new))
      (context-navigator--log "Project switch -> %s" (or root "~"))
      ;; Сразу публикуем список групп для нового root, чтобы исключить смешение
      (ignore-errors (context-navigator-groups-open))
      (when context-navigator-autoload
        (progn
          ;; Clear only gptel before loading context of the new project; keep old items visible
          ;; while loading (the View shows a spinner when loading-p is set).
          (when context-navigator--push-to-gptel
            ;; Avoid blocking UI by performing a clear asynchronously (bridge handles capability).
            (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-clear-all-now)))))
          (context-navigator--load-context-for-root root))))))


;;;###autoload
(defun context-navigator-context-load (&optional prompt)
  "Manually load context for current project or globally.
With PROMPT (prefix argument), prompt for a root directory; empty input = global."
  (interactive "P")
  ;; Prevent any pending autosave from firing between root change and inhibit flags.
  (ignore-errors (context-navigator-events-cancel :autosave))
  (let* ((root (if prompt
                   (let ((dir (read-directory-name "Load context for root (empty for global): " nil nil t)))
                     (and (stringp dir)
                          (not (string-empty-p (string-trim dir)))
                          (expand-file-name dir)))
                 (ignore-errors
                   (context-navigator-project-root (current-buffer))))))
    ;; Update last project root via a copy to avoid direct mutation; inhibit autosave/refresh while switching.
    (let* ((cur (context-navigator--state-get))
           (new (context-navigator--state-copy cur)))
      (setf (context-navigator-state-last-project-root new) root)
      (setf (context-navigator-state-inhibit-refresh new) t)
      (setf (context-navigator-state-inhibit-autosave new) t)
      (setf (context-navigator-state-loading-p new) t)
      (context-navigator--set-state new))
    ;; Clear only gptel before loading the new context; keep current items until load succeeds.
    (when context-navigator--push-to-gptel
      ;; Async clear to avoid blocking interactive manual load command (bridge handles capability).
      (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-clear-all-now)))))
    (context-navigator--log "Manual load -> %s" (or root "~"))
    (context-navigator--load-context-for-root root)))

;;;###autoload
(defun context-navigator-context-save ()
  "Manually save current model items to the active group's file for the active root."
  (interactive)
  (let* ((st (context-navigator--state-get))
         (items (and st (context-navigator-state-items st)))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st))))
    (if (and (stringp slug) (not (string-empty-p slug)))
        (let ((file (ignore-errors (context-navigator-persist-save items root slug))))
          (if file
              (context-navigator-ui-info :context-saved (abbreviate-file-name file))
            (context-navigator-ui-info :context-save-failed)))
      (context-navigator-ui-info :no-active-group))))

;;;###autoload
(defun context-navigator-context-clear-current-group ()
  "Clear all items in the active group and persist an empty context.
Also clears gptel context when push→gptel is enabled."
  (interactive)
  (let* ((st (context-navigator--state-get))
         (root (and st (context-navigator-state-last-project-root st)))
         (slug (and st (context-navigator-state-current-group-slug st))))
    (if (and (stringp slug) (not (string-empty-p slug)))
        (progn
          (context-navigator-set-items '())
          (ignore-errors (context-navigator-persist-save '() root slug))
          (when (and (boundp 'context-navigator--push-to-gptel)
                     context-navigator--push-to-gptel)
            (ignore-errors (context-navigator-clear-gptel-now)))
          (context-navigator-ui-info :cleared-current-group slug))
      (context-navigator-ui-info :no-active-group))))

(defun context-navigator--unload-begin ()
  "Begin unload sequence: cancel autosave and set inhibit flags + global root."
  (ignore-errors (context-navigator-events-cancel :autosave))
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur)))
    (setf (context-navigator-state-inhibit-refresh new) t)
    (setf (context-navigator-state-inhibit-autosave new) t)
    (setf (context-navigator-state-loading-p new) t)
    (setf (context-navigator-state-last-project-root new) nil)
    (context-navigator--set-state new)))

(defun context-navigator--unload-clear-gptel-async ()
  "Best-effort async clear of gptel context when push→gptel is enabled."
  (when context-navigator--push-to-gptel
    (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-gptel-clear-all-now))))))

(defun context-navigator--unload-clear-model ()
  "Clear the current model items (autosave is inhibited during unload)."
  (context-navigator-set-items '()))

(defun context-navigator--unload-finalize ()
  "Drop inhibit flags and mark loading finished after unload."
  (let* ((cur (context-navigator--state-get))
         (new (context-navigator--state-copy cur)))
    (setf (context-navigator-state-inhibit-refresh new) nil)
    (setf (context-navigator-state-inhibit-autosave new) nil)
    (setf (context-navigator-state-loading-p new) nil)
    (context-navigator--set-state new)))

;;;###autoload
(defun context-navigator-context-unload ()
  "Unload/clear context and switch to global (nil root).
Removes all gptel context entries and resets state flags safely."
  (interactive)
  (context-navigator--unload-begin)
  (context-navigator--unload-clear-gptel-async)
  (context-navigator--unload-clear-model)
  (context-navigator--unload-finalize)
  (context-navigator-events-publish :context-load-done nil nil)
  (context-navigator-ui-info :context-unloaded))

(defun context-navigator--on-model-refreshed (state)
  "Handle :model-refreshed by pushing to gptel when allowed."
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel
             (fboundp 'context-navigator-gptel-apply)
             (context-navigator--apply-allowed-p))
    (context-navigator-events-debounce
     :gptel-apply
     0.03
     (lambda ()
       (let ((st (ignore-errors (context-navigator--state-get))))
         (when (and (context-navigator-state-p st)
                    ;; Skip while loading to avoid fighting load-batch.
                    (not (context-navigator-state-loading-p st))
                    ;; Ensure we act on the latest generation we’ve seen.
                    (>= (or (context-navigator-state-generation st) 0)
                        (or (and (context-navigator-state-p state)
                                 (context-navigator-state-generation state))
                            0)))
           (let* ((root (ignore-errors (context-navigator-state-last-project-root st)))
                  (ps   (and root (ignore-errors (context-navigator-persist-state-load root))))
                  (mg   (and (listp ps) (plist-member ps :multi) (plist-get ps :multi)))
                  (sel  (ignore-errors (context-navigator--selected-group-slugs-for-root root))))
             (cond
              ;; MG ON + non-empty selection → push ALL items across selected groups (forced enabled)
              ((and mg (listp sel) (> (length sel) 0))
               (context-navigator-collect-items-for-groups-async
                root sel
                (lambda (items)
                  (let* ((token (and st (context-navigator-state-load-token st)))
                         (forced (context-navigator--force-enable-items items)))
                    (ignore-errors (context-navigator--gptel-defer-or-start (or forced '()) token))))))
              ;; MG ON + empty selection → clear gptel now
              (mg
               (ignore-errors (context-navigator-gptel-clear-all-now)))
              ;; MG OFF → apply current model (enabled-only semantics inside bridge)
              (t
               (let ((items (context-navigator-state-items st)))
                 (ignore-errors (context-navigator-gptel-apply (or items '())))))))))))))

(defun context-navigator--subscribe-project-switch ()
  "Subscribe to :project-switch and track the token."
  (push (context-navigator-events-subscribe :project-switch #'context-navigator--on-project-switch)
        context-navigator--event-tokens))

(defun context-navigator--subscribe-model-refreshed ()
  "Subscribe to :model-refreshed and track the token."
  (push (context-navigator-events-subscribe :model-refreshed #'context-navigator--on-model-refreshed)
        context-navigator--event-tokens))

(defun context-navigator--mode-enable ()
  "Enable Context Navigator: wiring, hooks, subscriptions."
  ;; Ensure global keybinding is applied when the mode is enabled
  (ignore-errors (context-navigator--update-global-keybinding))
  ;; Install project hooks
  (ignore-errors (context-navigator-project-setup-hooks))
  ;; Event subscriptions
  ;; gptel-change subscription disabled (no pull from gptel anymore)
  (context-navigator--subscribe-project-switch)
  (context-navigator--subscribe-model-refreshed)
  ;; Initial gptel sync disabled (Navigator no longer pulls from gptel)
  ;; If auto-project is already ON, trigger an initial project switch immediately.
  (when context-navigator--auto-project-switch
    (let ((root (ignore-errors (context-navigator--pick-root-for-autoproject))))
      (context-navigator-events-publish :project-switch root)))
  (context-navigator--log "mode enabled"))

(defun context-navigator--mode-disable ()
  "Disable Context Navigator: unsubscribe and teardown hooks."
  (mapc #'context-navigator-events-unsubscribe context-navigator--event-tokens)
  (setq context-navigator--event-tokens nil)
  (ignore-errors (context-navigator-project-teardown-hooks))
  (context-navigator--log "mode disabled"))

;;;###autoload
(define-minor-mode context-navigator-mode
  "Global mode for context-navigator (lightweight).
Sets up event wiring and keybindings."
  :init-value nil
  :global t
  :keymap context-navigator-mode-map
  (if context-navigator-mode
      (context-navigator--mode-enable)
    (context-navigator--mode-disable)))

(defun context-navigator-set-items (items)
  "Replace current model ITEMS with ITEMS and publish :model-refreshed.
Prunes dead buffer items (non-live buffers). Return the new state."
  (let* ((pruned
          (cl-remove-if
           (lambda (it)
             (and (eq (context-navigator-item-type it) 'buffer)
                  (not (buffer-live-p (context-navigator-item-buffer it)))))
           (or items '())))
         (removed (- (length (or items '())) (length pruned)))
         (cur (context-navigator--state-get))
         (new (context-navigator--state-with-items (context-navigator--state-copy cur) pruned)))
    (when (> removed 0)
      (context-navigator-ui-info :removed-dead-items removed))
    (context-navigator--set-state new)
    (context-navigator-events-publish :model-refreshed new)
    ;; Autosave only when items actually changed (no saves on pure refresh/reinit).
    (when (and context-navigator-autosave
               (context-navigator-state-p new)
               (not (context-navigator-state-inhibit-autosave new)))
      (let* ((old-items (and (context-navigator-state-p cur)
                             (context-navigator-state-items cur)))
             (diff (context-navigator-model-diff (or old-items '()) pruned))
             (changed (or (plist-get diff :add)
                          (plist-get diff :remove)
                          (plist-get diff :update)))
             (root (context-navigator-state-last-project-root new))
             (slug (context-navigator-state-current-group-slug new))
             (items (context-navigator-state-items new)))
        ;; Save only when a named group is active and there was a real change.
        (when (and changed
                   (stringp slug) (not (string-empty-p slug)))
          (ignore-errors (context-navigator-persist-save items root slug)))))
    new))

(defun context-navigator-add-item (item)
  "Add ITEM to the model (deduplicated by key; last wins). Return new state."
  (let* ((cur (context-navigator--state-get))
         (old (and cur (context-navigator-state-items cur)))
         (items (append old (list item)))
         ;; Deduplicate only here (API semantics: last wins); set-items
         ;; intentionally preserves order/duplicates for callers that need it.
         (uniq (context-navigator-model-uniq items)))
    (context-navigator-set-items uniq)))

(defun context-navigator-remove-item-by-key (key)
  "Remove item with stable KEY from the model. Return new state."
  (let* ((cur (context-navigator--state-get))
         (old (and cur (context-navigator-state-items cur)))
         (keep (cl-remove-if (lambda (it)
                               (string= (context-navigator-model-item-key it) key))
                             old)))
    (context-navigator-set-items keep)))

(defun context-navigator-toggle-item (key &optional enabled)
  "Toggle enabled flag for item with KEY. If ENABLED non-nil, set explicitly.
Return new state. If KEY not found, return current state."
  (let* ((cur (context-navigator--state-get))
         (idx (and cur (context-navigator-state-index cur)))
         (it  (and idx (gethash key idx))))
    (if (not (context-navigator-item-p it))
        cur
      (let* ((new-enabled (if (null enabled)
                              (not (context-navigator-item-enabled it))
                            (and enabled t)))
             (updated (context-navigator-item-create
                       :type (context-navigator-item-type it)
                       :name (context-navigator-item-name it)
                       :path (context-navigator-item-path it)
                       :buffer (context-navigator-item-buffer it)
                       :beg (context-navigator-item-beg it)
                       :end (context-navigator-item-end it)
                       :size (context-navigator-item-size it)
                       :enabled new-enabled
                       :meta (context-navigator-item-meta it)))
             (items (mapcar (lambda (x)
                              (if (string= (context-navigator-model-item-key x) key)
                                  updated x))
                            (context-navigator-state-items cur))))
        (context-navigator-set-items items)))))

;;;; Groups: moved to context-navigator-groups.el
;; All group CRUD and listing functions now live in `context-navigator-groups'.
;; Core requires that module at load time and calls its API directly.

(defun context-navigator--pick-root-for-autoproject ()
  "Return a project root for immediate auto-project activation.

Strategy:
- Prefer the project of the first file-visiting window on the current frame
  (stable, window-order based; avoids mixing contexts).
- Fallback to the most recently visited file-backed buffer's project.
- Fallback to the most recent \"interesting\" buffer (file/gptel/Dired).
- As a last resort, return nil (global context)."
  (or
   ;; 1) Frame-first, file-visiting window (stable/project.el-validated)
   (ignore-errors (context-navigator-project--frame-file-project-root))
   ;; 2) Last visited file-backed buffer's project
   (let* ((buf-file
           (cl-find-if (lambda (b) (with-current-buffer b buffer-file-name))
                       (buffer-list)))
          (root-file (and buf-file (context-navigator-project-root buf-file))))
     root-file)
   ;; 3) First interesting buffer's project (gptel/org/dired etc.)
   (let* ((buf-any (cl-find-if #'context-navigator-project--interesting-buffer-p
                               (buffer-list))))
     (and buf-any (context-navigator-project-root buf-any)))))

(defun context-navigator--reinit-resubscribe ()
  "Reinstall hooks and core event subscriptions after reload."
  ;; Unsubscribe only our own tokens; do not reset the global event bus
  ;; to avoid breaking other modules’ subscriptions (logs, UI, etc.).
  (mapc #'context-navigator-events-unsubscribe context-navigator--event-tokens)
  (setq context-navigator--event-tokens nil)
  ;; Ensure project hooks installed
  (ignore-errors (context-navigator-project-setup-hooks))
  ;; Re-subscribe core listeners
  (context-navigator--subscribe-project-switch)
  (context-navigator--subscribe-model-refreshed))

(defun context-navigator--reload-active-group-or-autoproject ()
  "Reload current group from disk (persisted :current); otherwise pick a reasonable project root."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (or (and st (ignore-errors (context-navigator-state-last-project-root st)))
                   (ignore-errors (context-navigator--pick-root-for-autoproject))))
         ;; Read :current from state.el when available; fall back to existing state's slug or \"default\"
         (ps (ignore-errors (context-navigator-persist-state-load root)))
         (slug (or (and st (ignore-errors (context-navigator-state-current-group-slug st)))
                   (and (listp ps) (plist-get ps :current))
                   "default")))
    (if (and (stringp slug) (not (string-empty-p slug)))
        (ignore-errors (context-navigator--load-group-for-root root slug))
      (when context-navigator--auto-project-switch
        (context-navigator-events-publish :project-switch root)))))

(defun context-navigator--reinit-after-reload ()
  "Reinstall hooks/subscriptions when file is reloaded and mode is ON.
Also reload the currently active group from disk (no clearing)."
  (when (bound-and-true-p context-navigator-mode)
    (context-navigator--reinit-resubscribe)
    (context-navigator--reload-active-group-or-autoproject)))

;;;###autoload
(defun context-navigator-restart ()
  "Hot-restart Context Navigator:
- close UI
- reset the event bus
- reload modules
- restore previous mode/UI visibility"
  (interactive)
  (let* ((was-mode (bound-and-true-p context-navigator-mode))
         (sidebar (ignore-errors (context-navigator--sidebar-visible-p)))
         (buffer  (ignore-errors (context-navigator--buffer-mode-visible-p)))
         (disp    (and (boundp 'context-navigator-display-mode) context-navigator-display-mode))
         ;; Preserve current context to restore after restart
         (prev-state (ignore-errors (context-navigator--state-get)))
         (prev-root (and prev-state (context-navigator-state-last-project-root prev-state)))
         (prev-slug (and prev-state (context-navigator-state-current-group-slug prev-state))))
    ;; Suppress empty saves across the whole restart window.
    (setq context-navigator-persist-suppress-empty-save t)
    ;; Best-effort autosave of current group before restart
    ;; Skip when a load is in progress to avoid saving a transient nil items.
    (let* ((st (ignore-errors (context-navigator--state-get)))
           (root (and st (context-navigator-state-last-project-root st)))
           (slug (and st (context-navigator-state-current-group-slug st)))
           (items (and st (context-navigator-state-items st)))
           (loading (and st (context-navigator-state-loading-p st))))
      (when (and (stringp slug) (not (string-empty-p slug))
                 (not loading)
                 (listp items))
        (ignore-errors (context-navigator-persist-save items root slug))))
    ;; Close UI
    (ignore-errors (context-navigator-view-close))
    (ignore-errors (context-navigator-buffer-close))
    ;; Disable mode
    (when was-mode (ignore-errors (context-navigator-mode -1)))
    ;; Reset event bus (clears dangling subscribers/timers)
    (ignore-errors (context-navigator-events-reset))
    ;; Soft-reload modules (safe order), keep core alive while we run
    (let ((mods '(context-navigator-events
                  context-navigator-fp
                  context-navigator-model
                  context-navigator-log
                  context-navigator-persist
                  context-navigator-project
                  context-navigator-gptel-bridge
                  context-navigator-render
                  context-navigator-icons
                  context-navigator-i18n
                  context-navigator-context-blocks
                  context-navigator-view-modeline
                  context-navigator-add-paths
                  context-navigator-which-key
                  context-navigator-transient
                  context-navigator-view)))
      (dolist (m mods)
        (condition-case _err
            (let ((lib (locate-library (symbol-name m))))
              (when lib (load lib t t)))
          (error nil))))
    ;; Restore mode (suppress initial auto-project to avoid races), then UI
    (when was-mode
      (let ((auto-on (and (boundp 'context-navigator--auto-project-switch)
                          context-navigator--auto-project-switch)))
        ;; Temporarily disable auto-project so `context-navigator--mode-enable'
        ;; doesn't publish an early :project-switch that can race our explicit load.
        (setq context-navigator--auto-project-switch nil)
        (ignore-errors (context-navigator-mode 1))
        ;; Restore the flag to its previous state.
        (setq context-navigator--auto-project-switch auto-on)))
    ;; Restore UI visibility close to previous state
    (pcase (cond (sidebar 'sidebar)
                 (buffer  'buffer)
                 (t       disp))
      ('sidebar (ignore-errors (context-navigator-view-open)))
      ('buffer  (ignore-errors (context-navigator-buffer-open)))
      (_ nil))
    ;; After restart, prefer restoring the previously active group directly when available.
    ;; This avoids briefly switching to another project or clearing the current group.
    (if (and (stringp prev-slug) (not (string-empty-p prev-slug)))
        (ignore-errors (context-navigator--load-group-for-root prev-root prev-slug))
      ;; Fallback: recover project context automatically by selecting the first
      ;; file-visiting buffer's project and publishing :project-switch.
      (let ((root (ignore-errors (context-navigator--pick-root-for-autoproject))))
        (when root
          (context-navigator-events-publish :project-switch root)
          (ignore-errors (context-navigator-groups-open)))))
    ;; Re-apply keys and which-key labels (if available) after reload.
    (when (fboundp 'context-navigator-keys-apply-known-keymaps)
      (ignore-errors (context-navigator-keys-apply-known-keymaps)))
    (when (fboundp 'context-navigator-which-key-apply!)
      (ignore-errors (context-navigator-which-key-apply!)))
    ;; Allow saves again after restart has finished wiring; loading-phase handlers
    ;; will clear the suppression flag once items are loaded.
    (setq context-navigator-persist-suppress-empty-save nil)
    (context-navigator-ui-info :restarted)))

;; --- Item open/selection-by-name ------------------------------------------------

(defun context-navigator--window-on-sidebar-p (&optional win)
  "Return non-nil if WIN (or selected window) is the Navigator sidebar window."
  (let ((w (or win (selected-window))))
    (and (window-live-p w)
         (eq (window-parameter w 'context-navigator-view) 'sidebar))))

(defun context-navigator-visit-item (item &optional prefer-other-window)
  "Open ITEM (file/buffer/selection). If PREFER-OTHER-WINDOW non-nil, open in other window.

When current window is the Navigator sidebar, open in other window by default.
For selection items, activate the region (mark active) after opening."
  (let* ((open-in-other (or prefer-other-window
                            (context-navigator--window-on-sidebar-p)))
         (safe-goto (lambda (pos)
                      (when (integerp pos)
                        (goto-char (min (max (point-min) pos) (point-max))))))
         (open-file (lambda (path)
                      (when (and (stringp path) (file-exists-p path))
                        (if open-in-other
                            (find-file-other-window path)
                          (find-file path)))))
         (switch-buf (lambda (buf)
                       (when (buffer-live-p buf)
                         (if open-in-other
                             (switch-to-buffer-other-window buf)
                           (switch-to-buffer buf))))))
    (pcase (context-navigator-item-type item)
      ('file
       (funcall open-file (context-navigator-item-path item)))
      ('buffer
       (let* ((buf (or (context-navigator-item-buffer item)
                       (and (context-navigator-item-path item)
                            (file-exists-p (context-navigator-item-path item))
                            (find-file-noselect (context-navigator-item-path item))))))
         (when buf (funcall switch-buf buf))))
      ('selection
       (let* ((path (context-navigator-item-path item))
              (buf (or (context-navigator-item-buffer item)
                       (and (stringp path) (file-exists-p path)
                            (find-file-noselect path))))
              (b (context-navigator-item-beg item))
              (e (context-navigator-item-end item)))
         (when buf
           (funcall switch-buf buf)
           (when (and (integerp b) (integerp e))
             (let ((beg (min b e)) (end (max b e)))
               (funcall safe-goto beg)
               (push-mark end t t)
               (when (fboundp 'recenter) (recenter)))))))
      (_ (context-navigator-ui-info :unknown-item-type)))))

(defun context-navigator--format-item-candidate (item)
  "Return a human-readable label for ITEM: \"<name> — <path-or-buffer>\"."
  (let* ((name (or (context-navigator-item-name item) ""))
         (path (context-navigator-item-path item))
         (buf  (context-navigator-item-buffer item))
         (disp (cond
                ((and (stringp path) (not (string-empty-p path)))
                 (let* ((st (ignore-errors (context-navigator--state-get)))
                        (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
                        (pp (condition-case _err
                                (if (and root (stringp root))
                                    (file-relative-name (expand-file-name path)
                                                        (file-name-as-directory (expand-file-name root)))
                                  (abbreviate-file-name (expand-file-name path)))
                              (error (abbreviate-file-name path)))))
                   pp))
                ((bufferp buf) (format "<%s>" (buffer-name buf)))
                (t ""))))
    (string-trim (if (and (stringp disp) (not (string-empty-p disp)))
                     (format "%s — %s" name disp)
                   (format "%s" name)))))

(defun context-navigator-select-by-name ()
  "Select a context item by name from all items in the current context and open it.

Uses consult when available (fuzzy search with live filtering),
falls back to `completing-read' otherwise.

Open in current window, or in another window when the current window is the Navigator sidebar.
For selections, activate the region after opening."
  (interactive)
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (ignore-errors (context-navigator-state-items st)))))
    (if (not (and (listp items) (> (length items) 0)))
        (context-navigator-ui-info :no-items-in-context)
      (let* ((base-alist
              (mapcar (lambda (it) (cons (context-navigator--format-item-candidate it) it))
                      items))
             ;; Disambiguate duplicate labels by appending a numeric suffix
             (counts (make-hash-table :test 'equal))
             (uniq-alist
              (mapcar
               (lambda (cell)
                 (let* ((lbl (car cell))
                        (cnt (1+ (gethash lbl counts 0))))
                   (puthash lbl cnt counts)
                   (if (> cnt 1)
                       (cons (format "%s (%d)" lbl cnt) (cdr cell))
                     cell)))
               base-alist))
             (prompt (context-navigator-i18n :select-item-prompt))
             (prefer-other (context-navigator--window-on-sidebar-p))
             (choice
              (cond
               ((and (require 'consult nil t) (fboundp 'consult--read))
                (consult--read (mapcar #'car uniq-alist)
                               :prompt prompt
                               :require-match t
                               :category 'context-navigator-item))
               (t
                (completing-read prompt (mapcar #'car uniq-alist) nil t)))))
        (when (and (stringp choice) (not (string-empty-p choice)))
          (let ((it (alist-get choice uniq-alist nil nil #'string=)))
            (when (context-navigator-item-p it)
              (context-navigator-visit-item it prefer-other))))))))

;; Auto-reinit after reload (eval-buffer/byte-compile)
(context-navigator--reinit-after-reload)


(provide 'context-navigator-core)
;;; context-navigator-core.el ends here
