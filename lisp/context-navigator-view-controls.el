;;; context-navigator-view-controls.el --- Controls (toolbar) for Navigator view -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Build toolbar controls (toggles + actions) for the Navigator view.
;; Split out of context-navigator-view.el to reduce coupling.
;;
;; This module does not require the full view to avoid cycles; it declares the
;; helpers it uses from the view. The view should
;; to expose the public controls API.
;;
;; Refactoring note:
;; - Controls are now described declaratively in a registry: logic + appearance
;; - Order/visibility is configured via a single order
;; - Rendering is unified and consumes registry + order

;;; Code:

(require 'cl-lib)
(require 'subr-x)
;; Keymap builder is provided by context-navigator-view (to avoid extra module).
(declare-function context-navigator-view-ui-make-keymap "context-navigator-view" (command &optional parent))
(require 'context-navigator-i18n)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-view-controls-icons)
(require 'context-navigator-keyspec)

;; Declarations to avoid load cycles; provided by context-navigator-view or core.
(declare-function context-navigator-view-open-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-close-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-clear-group "context-navigator-view-actions" ())
(declare-function context-navigator-view-push-now "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-all-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-push "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-auto-project "context-navigator-view-actions" ())
(declare-function context-navigator-undo "context-navigator-core" ())
(declare-function context-navigator-redo "context-navigator-core" ())

(declare-function context-navigator-view-razor-run "context-navigator-view-actions" ())
(declare-function context-navigator-multifile-open "context-navigator-multifile" ())
(declare-function context-navigator-stats-split-toggle "context-navigator-stats-split" ())
(declare-function context-navigator-stats-split-visible-p "context-navigator-stats-split" ())
(declare-function context-navigator-view-push-now-dispatch "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-toggle-multi-group "context-navigator-view-dispatch" ())
(declare-function context-navigator-persist-state-load "context-navigator-persist" (root))
(declare-function context-navigator-persist-group-enabled-count "context-navigator-persist" (file))
(declare-function context-navigator-persist-context-file "context-navigator-persist" (root &optional group-slug))
(declare-function context-navigator--state-get "context-navigator-core" ())
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-state-items "context-navigator-core" (state))
(declare-function context-navigator-state-current-group-slug "context-navigator-core" (state))

(defgroup context-navigator-view-controls nil
  "Toolbar controls (toggles and actions) for Context Navigator view."
  :group 'context-navigator)

(defface context-navigator-toolbar
  '((t :inherit default))
  "Default toolbar face for Context Navigator.
Inherit the theme's toolbar face (no custom background)."
  :group 'context-navigator-view-controls)

(defface context-navigator-view-controls-disabled-face
  '((t :inherit shadow))
  "Face for disabled controls.
Only changes foreground (no height/weight), so icon/text size stays stable."
  :group 'context-navigator-view-controls)

(defvar-local context-navigator--toolbar-face-cookie nil
  "Face-remap cookie for remapping the face in Navigator buffer.")

;; --- Keys fallback labels from keyspec (Middle Path) ------------------------

(defun context-navigator-view-controls--keyspec-first-key (id)
  "Return first key string from keyspec for action ID in current view context."
  (let* ((ctx (if (and (boundp 'context-navigator-view--mode)
                       (eq context-navigator-view--mode 'groups))
                  'groups
                'items)))
    (and (fboundp 'context-navigator-keys-first-key)
         (context-navigator-keys-first-key id ctx))))

(defun context-navigator-view-controls--keyspec-label (id default)
  "Return bracketed label \" [k]\" for ID from keyspec; fallback to DEFAULT.
DEFAULT should be a short key text like \"p\", \"x\", \"RET\", etc."
  (let* ((k (or (context-navigator-view-controls--keyspec-first-key id) default))
         (disp (cond
                ((null k) default)
                ((member k '("RET" "<return>" "<kp-enter>")) "RET")
                ((member k '("SPC")) "SPC")
                ((member k '("TAB" "<tab>" "C-i")) "TAB")
                ((member k '("<backtab>" "S-<tab>")) "S-TAB")
                (t k))))
    (format " [%s]" disp)))

(defun context-navigator-view-controls--after-control-invoke ()
  "Force immediate UI refresh after a control command."
  (let ((buf (get-buffer "*context-navigator*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Invalidate view and toolbar caches and refresh immediately.
        (setq-local context-navigator-render--last-hash nil)
        (setq-local context-navigator-view--last-render-key nil)
        (setq-local context-navigator-controls--cache-key nil)
        (setq-local context-navigator-controls--cache-str nil)
        ;; Rebuild modeline menu cache so redisplay doesn't compute controls.
        (when (fboundp 'context-navigator-modeline--rebuild-menu-cache)
          (ignore-errors (context-navigator-modeline--rebuild-menu-cache)))
        (force-mode-line-update t)
        (when (fboundp 'context-navigator-view--render-if-visible)
          (context-navigator-view--render-if-visible))))))

;; ---- Small pure helpers for gating push/autopush ---------------------------

(defun context-navigator-view-controls--selected-slugs ()
  "Return selected group slugs from state.el for current root (pure)."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (ps (and (stringp root)
                  (ignore-errors (context-navigator-persist-state-load root)))))
    (let ((sel (and (listp ps) (plist-member ps :selected) (plist-get ps :selected))))
      (if (listp sel) sel '()))))

(defun context-navigator-view-controls--sum-enabled-for-slugs (root slugs)
  "Return sum of enabled items across SLUGS (reads per-group v3 files)."
  (let ((sum 0))
    (dolist (slug slugs sum)
      (let* ((file (ignore-errors (context-navigator-persist-context-file root slug)))
             (en.t (and (stringp file)
                        (ignore-errors (context-navigator-persist-group-enabled-count file))))
             (en (and (consp en.t) (car en.t))))
        (when (integerp en)
          (setq sum (+ sum en)))))))

(defun context-navigator-view-controls--sum-total-for-slugs (root slugs)
  "Return sum of TOTAL items across SLUGS (reads per-group v3 files)."
  (let ((sum 0))
    (dolist (slug slugs sum)
      (let* ((file (ignore-errors (context-navigator-persist-context-file root slug)))
             (en.t (and (stringp file)
                        (ignore-errors (context-navigator-persist-group-enabled-count file))))
             (tot (and (consp en.t) (cdr en.t))))
        (when (integerp tot)
          (setq sum (+ sum tot)))))))

(defun context-navigator-view-controls--mg-active-p ()
  "Return non-nil if Multi-group mode is active for current root."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (ps (and (stringp root)
                  (ignore-errors (context-navigator-persist-state-load root)))))
    (and (listp ps) (plist-member ps :multi) (plist-get ps :multi))))

(defun context-navigator-view-controls--can-push-p ()
  "Return non-nil if pushing to gptel is allowed now.
In Multi-group mode: selection non-empty and aggregated total > 0.
Otherwise: current group has at least one enabled item."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st))))
    (if (context-navigator-view-controls--mg-active-p)
        (let* ((sel (context-navigator-view-controls--selected-slugs)))
          (and (listp sel) (> (length sel) 0)
               (> (context-navigator-view-controls--sum-total-for-slugs root sel) 0)))
      (context-navigator-view-controls--items-enabled-p))))

(defun context-navigator-view-controls--push-allowed-p ()
  "Return non-nil when push/auto should be enabled in groups mode.
Rule (MG): selection non-empty AND aggregated TOTAL items > 0."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st)))
         (sel (context-navigator-view-controls--selected-slugs)))
    (and (stringp root)
         (listp sel) (> (length sel) 0)
         (> (context-navigator-view-controls--sum-total-for-slugs root sel) 0))))

(defun context-navigator-view-controls--items-enabled-p ()
  "Return non-nil when current group (items mode) has at least one enabled item."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (items (and st (ignore-errors (context-navigator-state-items st)))))
    (and (listp items)
         (cl-some (lambda (it) (and (context-navigator-item-p it)
                                    (context-navigator-item-enabled it)))
                  items))))

(defun context-navigator-view-controls--push-disabled-reason ()
  "Return a localized reason string when push is disabled, or nil."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (context-navigator-state-last-project-root st))))
    (if (context-navigator-view-controls--mg-active-p)
        (let* ((sel (context-navigator-view-controls--selected-slugs)))
          (cond
           ((or (null (listp sel)) (= (length sel) 0))
            (context-navigator-i18n :no-group-selected))
           ((<= (context-navigator-view-controls--sum-total-for-slugs root sel) 0)
            (context-navigator-i18n :no-items-in-selection))
           (t nil)))
      (unless (context-navigator-view-controls--items-enabled-p)
        (context-navigator-i18n :razor-no-enabled-items)))))

;; Layout: order of controls for toolbar.
(defcustom context-navigator-toolbar-controls-order
  '(push auto-project multi-group :gap groups-split stats multifile :gap undo redo
         :gap toggle-all-gptel razor :gap push-now :gap open-buffers close-buffers
         :gap clear-group)
  "Controls order for the toolbar.
Remove a key to hide the control. You may also insert :gap for spacing."
  :type '(repeat (choice symbol (const :gap)))
  :group 'context-navigator-view-controls)

;; Registry: declarative descriptors for each control (logic + appearance).
;; Each entry is (KEY . plist), where plist supports:
;;  :type       'toggle | 'action
;;  :icon-key   symbol passed to context-navigator-view-controls-icon
;;  :command    function symbol to call on click (mouse-1)
;;  :help       string or (lambda () string)
;;  :enabled-p  (lambda () boolean)
;;  :visible-p  (lambda () boolean)
;;  :state-fn   (lambda () 'on | 'off)      ; only for :type 'toggle
;;  :spinner-fn (lambda () string-or-nil)   ; optional spinner
;;  :label-fn   (lambda (style state) string) ; fallback label when no icon/spinner
;;  :face-fn    (lambda (style state) face-or-plist) ; optional face for text fallback
(defcustom context-navigator-view-controls-registry
  (let ((tr #'context-navigator-i18n))
    `(
      (push
       :type toggle
       :icon-key push
       :command context-navigator-view-toggle-push
       :help ,(lambda ()
                (let ((base (funcall tr :toggle-push))
                      (why (context-navigator-view-controls--push-disabled-reason)))
                  (if why (format "%s — %s" base why) base)))
       :enabled-p ,(lambda ()
                     (and (ignore-errors (context-navigator-gptel-available-p))
                          (context-navigator-view-controls--can-push-p)))
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (if (and (boundp 'context-navigator--push-to-gptel)
                             context-navigator--push-to-gptel)
                        'on 'off))
       :label-fn ,(lambda (style state)
                    (pcase style
                      ((or 'icons 'auto) " [→]")
                      (_ (format " [gptel: %s]" (context-navigator-i18n (if (eq state 'on) :on :off)))))))
      (auto-project
       :type toggle
       :icon-key auto-project
       :command context-navigator-view-toggle-auto-project
       :help ,(lambda () (funcall tr :toggle-auto))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (if (and (boundp 'context-navigator--auto-project-switch)
                             context-navigator--auto-project-switch)
                        'on 'off))
       :label-fn ,(lambda (style state)
                    (pcase style
                      ((or 'icons 'auto) " [A]")
                      (_ (format " [%s: %s]" (context-navigator-i18n :auto-proj) (context-navigator-i18n (if (eq state 'on) :on :off))))))
       :face-fn ,(lambda (_style state)
                   ;; Only used when no graphic icons are available.
                   (list :foreground (if (eq state 'on) "green4" "gray"))))
      (undo
       :type action
       :icon-key undo
       :command context-navigator-undo
       :help ,(lambda () (funcall tr :razor-undo))
       :enabled-p ,(lambda () (fboundp 'context-navigator-undo))
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _state)
                    (pcase style
                      ((or 'icons 'auto) " [↶]")
                      (_ (concat " " (funcall tr :razor-undo) "")))))
      (redo
       :type action
       :icon-key redo
       :command context-navigator-redo
       :help ,(lambda () (funcall tr :razor-redo))
       :enabled-p ,(lambda () (fboundp 'context-navigator-redo))
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (pcase style
                      ((or 'icons 'auto) " [↷]")
                      (_ (concat " " (funcall tr :razor-redo) "")))))
      (razor
       :type action
       :icon-key razor
       :command context-navigator-view-razor-run
       :help ,(lambda () (funcall tr :tr-razor))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :spinner-fn ,(lambda ()
                      (and (boundp 'context-navigator-razor--running)
                           context-navigator-razor--running
                           (fboundp 'context-navigator-razor-spinner-frame)
                           (context-navigator-razor-spinner-frame)))
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (context-navigator-i18n :tr-razor))
                      " [R]")))
      (stats
       :type toggle
       :icon-key stats
       :command context-navigator-stats-split-toggle
       :help ,(lambda () (funcall tr :stats))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (if (and (fboundp 'context-navigator-stats-split-visible-p)
                             (context-navigator-stats-split-visible-p))
                        'on 'off))
       :label-fn ,(lambda (style _state)
                    (pcase style
                      ((or 'icons 'auto) " [Σ]")
                      (_ (format " [%s]" (context-navigator-i18n :stats)))))
       :face-fn ,(lambda (_style state)
                   (if (eq state 'on)
                       (list :foreground "MediumPurple1")
                     'shadow)))
      (groups-split
       :type toggle
       :icon-key groups-split
       :command context-navigator-groups-split-toggle
       :help ,(lambda () (funcall tr :toggle-groups-split))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (if (and (fboundp 'context-navigator-groups-split-visible-p)
                             (context-navigator-groups-split-visible-p))
                        'on 'off))
       :label-fn ,(lambda (style _state)
                    (pcase style
                      ((or 'icons 'auto) " [G]")
                      (_ (format " [%s]" (context-navigator-i18n :groups)))))
       :face-fn ,(lambda (_style state)
                   (if (eq state 'on)
                       (list :foreground "MediumOrchid3")
                     'shadow)))
      (multi-group
       :type toggle
       :icon-key multi-group
       :command context-navigator-view-toggle-multi-group
       :help ,(lambda () (funcall tr :toggle-multi-group))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :state-fn ,(lambda ()
                    (let* ((st (ignore-errors (context-navigator--state-get)))
                           (root (and st (context-navigator-state-last-project-root st)))
                           (ps (and (stringp root)
                                    (ignore-errors (context-navigator-persist-state-load root))))
                           (mg (and (listp ps) (plist-member ps :multi) (plist-get ps :multi))))
                      (if mg 'on 'off)))
       :label-fn ,(lambda (style state)
                    (if (eq style 'text)
                        (if (eq state 'on) " [MG*]" " [MG]")
                      (if (eq state 'on) " [MG✓]" " [MG]"))))
      (push-now
       :type action
       :icon-key push-now
       :command context-navigator-view-push-now
       :help ,(lambda ()
                (let ((base (funcall tr :push-now))
                      (why (context-navigator-view-controls--push-disabled-reason)))
                  (if why (format "%s — %s" base why) base)))
       :enabled-p ,(lambda ()
                     (context-navigator-view-controls--can-push-p))
       :visible-p ,(lambda () t)
       :spinner-fn ,(lambda ()
                      (and (boundp 'context-navigator-view--gptel-batch-start-time)
                           context-navigator-view--gptel-batch-start-time
                           (boundp 'context-navigator-view--spinner-index)
                           (boundp 'context-navigator-view-spinner-frames)
                           (let* ((frames context-navigator-view-spinner-frames)
                                  (idx (or context-navigator-view--spinner-index 0))
                                  (len (length (or frames '()))))
                             (when (> len 0)
                               ;; Two-character frame so segment width doesn't "jump"
                               (let ((fr (nth (mod idx len) frames)))
                                 (concat fr fr))))))
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :push-now)))
                      (context-navigator-view-controls--keyspec-label 'push-now "p"))))
      (open-buffers
       :type action
       :icon-key open-buffers
       :command context-navigator-view-open-all-buffers
       :help ,(lambda () (funcall tr :open-buffers))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :open-buffers)))
                      (context-navigator-view-controls--keyspec-label 'open-buffers "o"))))
      (close-buffers
       :type action
       :icon-key close-buffers
       :command context-navigator-view-close-all-buffers
       :help ,(lambda () (funcall tr :close-buffers))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :close-buffers)))
                      (context-navigator-view-controls--keyspec-label 'close-buffers "c"))))

      (clear-group
       :type action
       :icon-key clear-group
       :command context-navigator-view-clear-group
       :help ,(lambda () (funcall tr :clear-group))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :clear-group)))
                      (context-navigator-view-controls--keyspec-label 'clear-group "x"))))
      (toggle-all-gptel
       :type action
       :icon-key toggle-all-gptel
       :command context-navigator-view-toggle-all-gptel
       :help ,(lambda () (funcall tr :toggle-all-gptel))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (if (eq style 'text)
                        (format " [%s]" (capitalize (context-navigator-i18n :toggle-all-gptel)))
                      (context-navigator-view-controls--keyspec-label 'toggle-all "T"))))
      (multifile
       :type action
       :icon-key multifile
       :command context-navigator-multifile-open
       :help ,(lambda () (funcall tr :tr-multifile))
       :enabled-p ,(lambda () t)
       :visible-p ,(lambda () t)
       :label-fn ,(lambda (style _s)
                    (pcase style
                      ((or 'icons 'auto) " [MF]")
                      (_ " [Multifile]"))))
      ))
  "Registry of Navigator controls for toolbar."
  :type '(alist :key-type symbol :value-type plist)
  :group 'context-navigator-view-controls)

(defun context-navigator-view-controls--plist-fn (val)
  "If VAL is a function, call it with no args, else return VAL."
  (if (functionp val) (funcall val) val))

(defun context-navigator-view-controls--render (key)
  "Render a single control segment for KEY using the controls registry.
Returns a propertized string or nil when not visible."
  (let* ((desc (alist-get key context-navigator-view-controls-registry))
         (type (plist-get desc :type))
         (cmd  (plist-get desc :command))
         (help (plist-get desc :help))
         (enabled-p (let ((fn (or (plist-get desc :enabled-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (visible-p (let ((fn (or (plist-get desc :visible-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (style (if (display-graphic-p)
                    (or context-navigator-view-controls-style 'auto)
                  'text)))
    (when (and desc visible-p)
      (let* ((gicons (and (fboundp 'context-navigator-view-controls-icons-available-p)
                          (context-navigator-view-controls-icons-available-p)))
             (state (when (eq type 'toggle)
                      (let ((fn (plist-get desc :state-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn))))))
             (spinner (let ((fn (plist-get desc :spinner-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn)))))
             (icon-key (plist-get desc :icon-key))
             (ico (and gicons (not spinner)
                       (context-navigator-view-controls-icon icon-key state)))
             (label (cond
                     (spinner (concat " " spinner))
                     (ico     (concat " " ico))
                     (t (let ((lf (plist-get desc :label-fn)))
                          (when (functionp lf) (funcall lf style state))))))
             ;; Ensure we work on a writable string copy
             (s (copy-sequence (or label "")))
             (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0))
             (km (let ((wrapped
                        (and cmd enabled-p
                             (let ((orig cmd))
                               (lambda ()
                                 (interactive)
                                 (call-interactively orig)
                                 (context-navigator-view-controls--after-control-invoke))))))
                   (when wrapped
                     (ignore-errors (context-navigator-view-ui-make-keymap wrapped)))))
             (help-str (context-navigator-view-controls--plist-fn help)))
        (when (> (length s) 0)
          (let ((props (list 'mouse-face 'highlight
                             'help-echo help-str
                             'context-navigator-key key
                             'context-navigator-interactive t)))
            (when km
              (setq props (append props (list 'keymap km 'local-map km))))
            (when (eq type 'toggle)
              (setq props (append props (list 'context-navigator-toggle key))))
            (when (eq type 'action)
              (setq props (append props (list 'context-navigator-action key))))
            (add-text-properties beg (length s) props s))
          ;; Apply optional face for textual fallback when icons are not used.
          (when-let* ((ff (plist-get desc :face-fn))
                      (face (and (functionp ff) (funcall ff style state))))
            (unless gicons
              (add-text-properties beg (length s)
                                   (list 'face (or (and (symbolp face) face)
                                                   (and (listp face) face)))
                                   s)))
          ;; Dim disabled without changing size using a dedicated face (foreground only).
          (unless enabled-p
            ;; Prepend the face so its foreground overrides icon/text colors.
            (add-face-text-property beg (length s)
                                    'context-navigator-view-controls-disabled-face
                                    nil s)))
        (and (> (length s) 0) s)))))

(defun context-navigator-view-controls-segments (&optional _where)
  "Return ordered control segments for the controls as a list of strings."
  (let* ((order context-navigator-toolbar-controls-order)
         (res '()))
    (dolist (k order)
      (if (eq k :gap)
          (push " " res)
        (when-let* ((seg (context-navigator-view-controls--render k)))
          (when (stringp seg)
            (push seg res)))))
    (nreverse res)))

;; Auto-refresh UI when registry or order variables change.
(when (fboundp 'add-variable-watcher)
  (dolist (sym '(context-navigator-toolbar-controls-order
                 context-navigator-view-controls-registry))
    (add-variable-watcher
     sym
     (lambda (&rest _)
       ;; Avoid full model refresh; do a local sidebar re-render
       (ignore-errors
         (let ((buf (get-buffer "*context-navigator*")))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               ;; Invalidate view and toolbar caches
               (setq-local context-navigator-render--last-hash nil)
               (setq-local context-navigator-view--last-render-key nil)
               (setq-local context-navigator-controls--cache-key nil)
               (setq-local context-navigator-controls--cache-str nil)
               ;; Rebuild modeline menu cache on structural changes
               (when (fboundp 'context-navigator-modeline--rebuild-menu-cache)
                 (ignore-errors (context-navigator-modeline--rebuild-menu-cache)))
               (when (fboundp 'context-navigator-view--render-if-visible)
                 (context-navigator-view--render-if-visible))))))
       (force-mode-line-update t)))))

(defun context-navigator-view-controls--wrap-segments (segments total-width)
  "Wrap SEGMENTS (list of strings) into lines within TOTAL-WIDTH columns.

Guarantees each returned line's visual width does not exceed TOTAL-WIDTH.
Segments longer than TOTAL-WIDTH are soft-split using `truncate-string-to-width'."
  (let* ((tw (max 1 (or total-width 80)))
         (acc '())
         (cur ""))
    (dolist (seg segments)
      (let ((seg (or seg "")))
        (while (and (stringp seg) (> (length seg) 0))
          (let* ((cw (string-width cur))
                 (avail (max 0 (- tw cw))))
            (cond
             ;; No space left on current line: emit it and continue
             ((= avail 0)
              (when (> (length cur) 0) (push cur acc))
              (setq cur ""))
             ;; Segment fits entirely on current line
             ((<= (string-width seg) avail)
              (setq cur (concat cur seg))
              (setq seg ""))
             ;; Need to split the segment to fit the remaining space
             (t
              (let* ((head (truncate-string-to-width seg avail nil nil))
                     (head-len (length head)))
                ;; If avail is small but truncate returns empty, force a line break
                (if (or (null head) (= head-len 0))
                    (progn
                      (when (> (length cur) 0) (push cur acc))
                      (setq cur "")) ;; retry with same seg on next loop
                  (setq cur (concat cur head))
                  (push cur acc)
                  (setq cur "")
                  ;; Remainder of seg (drop the chars we consumed)
                  (setq seg (substring seg head-len))))))))))
    (when (> (length cur) 0)
      (push cur acc))
    (nreverse acc)))

(defun context-navigator-view-controls-lines (total-width)
  "Return toolbar control lines wrapped to TOTAL-WIDTH columns.

When TOTAL-WIDTH is nil or non-positive, try to use the selected window width;
fallback to 80 columns."
  (let* ((tw (cond
              ((and (numberp total-width) (> total-width 0)) total-width)
              ((window-live-p (selected-window)) (window-body-width (selected-window)))
              (t 80)))
         (segments (context-navigator-view-controls-segments)))
    (context-navigator-view-controls--wrap-segments (or segments '()) tw)))

(provide 'context-navigator-view-controls)
;;; context-navigator-view-controls.el ends here
