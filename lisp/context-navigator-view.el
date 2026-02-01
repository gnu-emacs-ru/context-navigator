;;; context-navigator-view.el --- Sidebar UI (side window) -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: MIT
;;; Commentary:
;; Lightweight, event-driven sidebar:
;; - Opens in a left side window with configurable width
;; - Subscribes to :model-refreshed and :context-load-(start|done)
;; - Renders via context-navigator-render, optional icons
;; - Minimal keymap: RET to visit, d delete, g refresh, q quit
;;
;; Functional by design: no state mutation outside buffer-local vars for UI.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-compat)
(require 'seq)
(require 'context-navigator-core)
(require 'context-navigator-events)
(require 'context-navigator-render)
(require 'context-navigator-model)
(require 'context-navigator-gptel-bridge)
(require 'context-navigator-icons)
(require 'context-navigator-persist)
(require 'context-navigator-i18n)
(require 'context-navigator-log)
(require 'context-navigator-view-modeline)
(require 'context-navigator-stats)
(require 'context-navigator-stats-split)
(ignore-errors (require 'context-navigator-groups-split nil t))
(require 'context-navigator-view-actions)
(require 'context-navigator-view-buffer)
(require 'context-navigator-view-constants)
(require 'context-navigator-view-controls)
(require 'context-navigator-view-counters)
;; defer dispatch: autoloaded when invoked; avoid early require to prevent load errors
(require 'context-navigator-view-events)
(require 'context-navigator-view-groups)
(require 'context-navigator-view-help)
(require 'context-navigator-view-indicators)
(require 'context-navigator-view-items)
(require 'context-navigator-view-navigation)
(require 'context-navigator-view-spinner)
;; moved: UI keymap helpers now live here (no separate segments module)
(require 'context-navigator-keyspec)
(require 'context-navigator-view-title)


;; --- UI keymap helpers (moved from context-navigator-view-segments.el) -------

(defvar context-navigator-view-ui--fallback-parent nil
  "Fallback parent keymap for Navigator interactive segments when the major mode map is not yet available.")

(defun context-navigator-view-ui-parent-keymap ()
  "Return a reliable parent keymap for Navigator UI segments.
Prefers `context-navigator-view-mode-map' when it is bound and a keymap.
Falls back to a lazily created sparse keymap cached in
`context-navigator-view-ui--fallback-parent'."
  (or (and (boundp 'context-navigator-view-mode-map)
           (keymapp context-navigator-view-mode-map)
           context-navigator-view-mode-map)
      (progn
        (unless (keymapp context-navigator-view-ui--fallback-parent)
          (setq context-navigator-view-ui--fallback-parent (make-sparse-keymap)))
        context-navigator-view-ui--fallback-parent)))

(defun context-navigator-view-ui-make-keymap (command &optional parent)
  "Return a sparse keymap for interactive segment bound to COMMAND.
When PARENT is a keymap, inherit it so navigation keys (n/p, j/k, arrows)
continue to work within the segment. If PARENT is nil or not a keymap,
inherit from a reliable Navigator parent keymap."
  (let* ((m (make-sparse-keymap))
         (p (or (and (keymapp parent) parent)
                (context-navigator-view-ui-parent-keymap))))
    (when (keymapp p)
      (set-keymap-parent m p))
    ;; Mouse clicks (regular, header-line and mode-line areas)
    (define-key m [mouse-1] command)
    (define-key m [header-line mouse-1] command)
    (define-key m [mode-line mouse-1] command)
    ;; RET variants
    (define-key m (kbd "RET")      command)
    (define-key m (kbd "C-m")      command)
    (define-key m [return]         command)
    (define-key m (kbd "<return>") command)
    ;; TAB variants (handy for toggles)
    (define-key m (kbd "TAB")   command)
    (define-key m (kbd "<tab>") command)
    (define-key m [tab]         command)
    (define-key m (kbd "C-i")   command)
    m))

(defcustom context-navigator-auto-open-groups-on-error t
  "When non-nil, automatically switch the sidebar to the groups list if a group fails to load."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-highlight-active-group t
  "When non-nil, highlight the active group in the groups list."
  :type 'boolean :group 'context-navigator)

(defcustom context-navigator-view-controls-style 'icons
  "Style for sidebar controls (toggles and footer buttons):
- auto  : prefer compact icon-like labels when possible
- icons : force compact icon-like labels
- text  : verbose text labels"
  :type '(choice (const auto) (const icons) (const text))
  :group 'context-navigator)

(defcustom context-navigator-view-diagnostics-events
  '(:model-refreshed :gptel-change :context-load-start :context-load-done
                     :group-switch-start :group-switch-done :groups-list-updated :group-selection-changed)
  "Events to monitor when diagnostics trace is active."
  :type '(repeat symbol)
  :group 'context-navigator)

(defcustom context-navigator-view-diagnostics-args-limit 1
  "Maximum number of event arguments to record in diagnostics trace.
Set to 0 or nil to skip capturing arguments (only arity is logged)."
  :type '(choice (const :tag "Record none" 0)
                 (const :tag "Record none" nil)
                 integer)
  :group 'context-navigator)

(defcustom context-navigator-view-diagnostics-arg-string-limit 160
  "Maximum number of characters recorded per event argument in diagnostics trace.
Set to nil to disable truncation."
  :type '(choice (const :tag "Do not truncate" nil)
                 integer)
  :group 'context-navigator)

(defcustom context-navigator-view-diagnostics-trace-max 200
  "Maximum number of entries kept in diagnostics trace log."
  :type 'integer
  :group 'context-navigator)

(defcustom context-navigator-view-diagnostics-include-backtrace nil
  "When non-nil, capture a trimmed backtrace for each scheduled render in diagnostics trace."
  :type 'boolean
  :group 'context-navigator)

(defcustom context-navigator-view-diagnostics-message-events nil
  "When non-nil, echo tracked events in the minibuffer while diagnostics trace is active."
  :type 'boolean
  :group 'context-navigator)

(defcustom context-navigator-view-diagnostics-event-window 1.0
  "Time window (seconds) to relate a scheduled render to the most recent tracked event."
  :type 'number
  :group 'context-navigator)

(defcustom context-navigator-gptel-indicator-poll-interval 0
  "Polling interval (seconds) to refresh gptel indicators while the sidebar is visible.

Set to 0 or nil to disable polling (event-based refresh still works)."
  :type 'number :group 'context-navigator)

;; Forward declarations to avoid load cycle; core provides these.
(declare-function context-navigator--state-get "context-navigator-core")
(declare-function context-navigator-state-last-project-root "context-navigator-core" (state))
(declare-function context-navigator-state-loading-p "context-navigator-core" (state))
(declare-function context-navigator-state-items "context-navigator-core" (state))
(declare-function context-navigator-state-index "context-navigator-core" (state))
(declare-function context-navigator-state-current-group-slug "context-navigator-core" (state))
(declare-function context-navigator-state-p "context-navigator-core" (state))
(declare-function context-navigator-state-generation "context-navigator-core" (state))
(declare-function context-navigator-toggle-item "context-navigator-core" (key &optional enabled))
(declare-function context-navigator-remove-item-by-key "context-navigator-core" (key))
(declare-function context-navigator-context-clear-current-group "context-navigator-core" ())
(declare-function context-navigator-context-unload "context-navigator-core" ())
(declare-function context-navigator-groups-open "context-navigator-groups" ())
(declare-function context-navigator-group-switch "context-navigator-groups" (&optional slug))
(declare-function context-navigator-group-create "context-navigator-groups" (&optional display-name))
(declare-function context-navigator-group-rename "context-navigator-groups" (&optional old-slug new-display))
(declare-function context-navigator-group-delete "context-navigator-groups" (&optional slug))
(declare-function context-navigator-group-duplicate "context-navigator-groups" (&optional src-slug new-display))
(declare-function context-navigator-group-edit-description "context-navigator-groups" (&optional slug new-desc))
(declare-function context-navigator--buffer-mode--split "context-navigator-view-buffer" (direction size))
(declare-function context-navigator-buffer-open "context-navigator-view-buffer" ())
(declare-function context-navigator-buffer-close "context-navigator-view-buffer" ())
(declare-function context-navigator-buffer-toggle "context-navigator-view-buffer" ())
(declare-function context-navigator-view-controls-segments "context-navigator-view-controls" ())
(declare-function context-navigator-view-controls-lines "context-navigator-view-controls" (total-width))
(declare-function context-navigator-view-counters-get-openable "context-navigator-view-counters" ())
(declare-function context-navigator-view-counters-refresh-openable "context-navigator-view-counters" ())
(declare-function context-navigator-view-counters-collect-closable "context-navigator-view-counters" ())
(declare-function context-navigator-view-counters-invalidate "context-navigator-view-counters" ())
(declare-function context-navigator-view-groups-header-lines "context-navigator-view-groups" (header total-width))
(declare-function context-navigator-view-groups-body-lines "context-navigator-view-groups" (state))
(declare-function context-navigator-view--groups-help-lines "context-navigator-view-groups" (total-width))
(declare-function context-navigator-view-render-groups "context-navigator-view-groups" (state header total-width))
(declare-function context-navigator-view-items-header-lines "context-navigator-view-items" (total-width))
(declare-function context-navigator-view--items-base-lines "context-navigator-view-items" (state header total-width))
(declare-function context-navigator-view--status-text-at-point "context-navigator-view-items" ())
(declare-function context-navigator-view--items-extra-lines "context-navigator-view-items" (total-width))
(declare-function context-navigator-view-render-items "context-navigator-view-items" (state header total-width))
(declare-function context-navigator-view--find-next-interactive-pos "context-navigator-view-navigation" (&optional start))
(declare-function context-navigator-view--find-prev-interactive-pos "context-navigator-view-navigation" (&optional start))
(declare-function context-navigator-view--find-next-itemish-pos "context-navigator-view-navigation" (&optional start))
(declare-function context-navigator-view--find-prev-itemish-pos "context-navigator-view-navigation" (&optional start))
(declare-function context-navigator-view--move-next-interactive "context-navigator-view-navigation" ())
(declare-function context-navigator-view-tab-next "context-navigator-view-navigation" ())
(declare-function context-navigator-view-tab-previous "context-navigator-view-navigation" ())
(declare-function context-navigator-view-toggle-enabled "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-delete-from-model "context-navigator-view-actions" ())
(declare-function context-navigator-view-open-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-close-all-buffers "context-navigator-view-actions" ())
(declare-function context-navigator-view-clear-group "context-navigator-view-actions" ())
(declare-function context-navigator-view-clear-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-enable-all-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-disable-all-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-all-gptel "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-push "context-navigator-view-actions" ())
(declare-function context-navigator-view-toggle-auto-project "context-navigator-view-actions" ())
(declare-function context-navigator-view-push-now "context-navigator-view-actions" ())
(declare-function context-navigator-view-razor-run "context-navigator-view-actions" ())
(declare-function context-navigator-view--subscribe-model-events   "context-navigator-view-events" ())
(declare-function context-navigator-view--subscribe-load-events    "context-navigator-view-events" ())
(declare-function context-navigator-view--subscribe-groups-events  "context-navigator-view-events" ())
(declare-function context-navigator-view--subscribe-project-events "context-navigator-view-events" ())
(declare-function context-navigator-view-events-install "context-navigator-view-events" ())
(declare-function context-navigator-view-events-remove "context-navigator-view-events" ())
(declare-function context-navigator-view--subscribe-gptel-events "context-navigator-view-indicators" ())
(declare-function context-navigator-view--track-cursor-post-cmd "context-navigator-view-events" ())
(declare-function context-navigator-view--save-items-cursor-state "context-navigator-view-events" ())
(declare-function context-navigator-view--init-gptel-cache       "context-navigator-view-indicators" ())
(declare-function context-navigator-view--start-gptel-poll-timer "context-navigator-view-indicators" ())
(declare-function context-navigator-view-help "context-navigator-view-help" ())
(declare-function context-navigator-view-open-menu "context-navigator-view-help" ())
(declare-function context-navigator-view-activate "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-refresh-dispatch "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-delete-dispatch "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-go-up "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-group-create "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-group-rename "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-group-duplicate "context-navigator-view-dispatch" (&optional src-slug new-display))
(declare-function context-navigator-view-group-edit-description "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-group-toggle-select "context-navigator-view-dispatch" ())
(declare-function context-navigator-view-push-now-dispatch "context-navigator-view-dispatch" ())

;; Lightweight autoloads for dispatchers so keyspec can bind before files load
;; (works even when the umbrella file wasn't required yet).
(autoload 'context-navigator-view-activate            "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-toggle-dispatch     "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-refresh-dispatch    "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-delete-dispatch     "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-go-up               "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-group-create        "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-group-rename        "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-group-duplicate     "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-group-edit-description "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-group-toggle-select "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-push-now-dispatch   "context-navigator-view-dispatch" nil t)
(autoload 'context-navigator-view-stats-toggle "context-navigator-view" nil t)

(defvar context-navigator-view--diag-trace-on nil)
(defvar context-navigator-view--diag-trace-log nil)
(defvar context-navigator-view--diag-trace-tokens nil)
(defvar context-navigator-view--diag-last-event nil)
(defconst context-navigator-view--diag-trace-buffer "*Context Navigator Render Trace*")

(defvar-local context-navigator-view--subs nil)
(defvar-local context-navigator-view--header "Context")
(defvar-local context-navigator-view--mode 'items) ;; 'items | 'groups
(defvar-local context-navigator-view--groups nil)  ;; cached groups plists
(defvar-local context-navigator-view--last-lines nil)
(defvar-local context-navigator-view--load-progress nil) ;; cons (POS . TOTAL) | nil)
(defvar-local context-navigator-view--winselect-fn nil)  ;; function added to window-selection-change-functions
(defvar-local context-navigator-view--gptel-keys nil)    ;; cached stable keys from gptel (for indicators)
(defvar-local context-navigator-view--gptel-keys-hash nil) ;; reserved for future use
(defvar-local context-navigator-view--sorted-items nil)  ;; cached sorted items (list) for current generation
(defvar-local context-navigator-view--sorted-gen nil)    ;; generation number of the cached sorted items
(defvar-local context-navigator-view--openable-count nil)          ;; cached count (int) or nil
(defvar-local context-navigator-view--openable-plus nil)           ;; non-nil when soft-cap reached
(defvar-local context-navigator-view--openable-stamp 0.0)          ;; float-time of last compute
(defvar-local context-navigator-view--openable-timer nil)          ;; pending timer for recompute
(defvar-local context-navigator-view--buflist-fn nil)              ;; function added to buffer-list-update-hook
(defvar-local context-navigator-view--gptel-poll-timer nil)        ;; polling timer for gptel indicators (or nil)
(defvar-local context-navigator-view--status-post-cmd-fn nil)      ;; post-command hook to update inline status line
(defvar-local context-navigator-view--last-render-key nil)        ;; cached render key to skip redundant renders
(defvar-local context-navigator-view--last-active-group nil)      ;; last active group cached to avoid jumping cursor in groups view
(defvar-local context-navigator-view--last-mode nil)              ;; last rendered mode: 'items or 'groups
(defvar-local context-navigator-view--sorted-root nil)            ;; root used for cached items sort
(defvar-local context-navigator-view--relpaths-hash nil)          ;; cache: item-key -> relpath for current generation/root
(defvar-local context-navigator-view--collapsed-p nil)            ;; when non-nil, hide everything below the title (TAB toggles)
(defvar-local context-navigator-view--sticky-item-key nil)        ;; sticky: stable key of item to keep point on after re-render
(defvar-local context-navigator-view--sticky-window-start nil)    ;; sticky: window-start to restore after re-render
(defvar-local context-navigator-view--restore-once nil)           ;; one-shot: force cursor restore on next items render
(defvar-local context-navigator-view--last-cursor-key nil)        ;; last observed cursor anchor (item key or "..")
(defvar-local context-navigator-view--last-modeline-key nil)      ;; last modeline anchor we rendered
(defvar-local context-navigator-view--cursor-post-cmd-fn nil)     ;; post-command hook to track cursor anchor

(defun context-navigator-view-toggle-collapse-immediate ()
  "Toggle collapse and render immediately."
  (interactive)
  (context-navigator-view-toggle-collapse)
  (context-navigator-view--render-if-visible))

(defvar-local context-navigator-view--spinner-timer nil)          ;; loading spinner timer
(defvar-local context-navigator-view--spinner-index 0)
(defvar-local context-navigator-view--spinner-last-time 0.0)      ;; last tick timestamp (float-time)
(defvar-local context-navigator-view--spinner-degraded nil)       ;; when non-nil, render static indicator
(defvar-local context-navigator--headerline-face-cookie nil)      ;; face-remap cookie for header-line

;; --- Live filter (name/content) state ---------------------------------------
(defvar-local context-navigator-view--filter-mode nil)        ;; nil | 'name | 'content
(defvar-local context-navigator-view--filter-query nil)       ;; current query string
(defvar-local context-navigator-view--filter-last-count nil)  ;; filtered count
(defvar-local context-navigator-view--filter-last-total nil)  ;; total count
(defvar-local context-navigator-view--filter-results nil)     ;; hash-set of matched item keys for content filter
(defvar-local context-navigator-view--content-run-id 0)       ;; incremental token for debounced scans

(defun context-navigator-view--filter-active-p ()
  "Return non-nil when a filter is active and the query is non-empty."
  (and (memq context-navigator-view--filter-mode '(name content))
       (stringp context-navigator-view--filter-query)
       (> (length (string-trim context-navigator-view--filter-query)) 0)))

(defvar context-navigator-view--filter-clear-keymap
  (when (fboundp 'context-navigator-view-ui-make-keymap)
    (context-navigator-view-ui-make-keymap 'context-navigator-view-filter-clear))
  "Clickable keymap for the [×] filter-clear icon in the header-line.")

(defun context-navigator-view--filter-header-segment ()
  "Return header-line segment for active filter with a clickable clear icon."
  (when (context-navigator-view--filter-active-p)
    (let* ((mode (if (eq context-navigator-view--filter-mode 'name) "S" "F"))
           (q (or context-navigator-view--filter-query ""))
           (n (or context-navigator-view--filter-last-count 0))
           (tot (or context-navigator-view--filter-last-total 0))
           (x-label (if (display-graphic-p) "[×]" "[X]"))
           (x (propertize x-label
                          'mouse-face 'mode-line-highlight
                          'help-echo (and (fboundp 'context-navigator-i18n)
                                          (context-navigator-i18n :filter-clear))
                          'keymap context-navigator-view--filter-clear-keymap
                          'local-map context-navigator-view--filter-clear-keymap))
           (txt (format " %s %s: %s %d/%d" x mode q (max 0 n) (max 0 tot))))
      (propertize txt 'face 'context-navigator-title-face))))

(defun context-navigator-view--keyboard-quit-or-clear ()
  "C-g: clear active filter when present; otherwise perform normal keyboard-quit."
  (interactive)
  (if (context-navigator-view--filter-active-p)
      (call-interactively 'context-navigator-view-filter-clear)
    (call-interactively 'keyboard-quit)))

(defcustom context-navigator-view-header-props
  '(context-navigator-header)
  "List of text-properties that mark section headers in the sidebar."
  :type '(repeat symbol) :group 'context-navigator)

(defcustom context-navigator-view-element-props
  '(context-navigator-interactive)
  "List of text-properties that are considered section elements for j/n/k/p."
  :type '(repeat symbol) :group 'context-navigator)

(defun context-navigator-view--header (state)
  "Compute compact header title from STATE (robust against stale structs).

Rules:
- Items mode: [<project>: <group>] when group is active, otherwise [<project>]
- Groups mode: [<project>] only
- Global (no project): use ~ as project name → items: [~: <group>] / groups: [~]

Note: status toggles [→gptel:on/off] [auto-proj:on/off] are rendered in the header-line."
  (let* ((root (and (context-navigator-state-p state)
                    (ignore-errors (context-navigator-state-last-project-root state))))
         (group (and (context-navigator-state-p state)
                     (ignore-errors (context-navigator-state-current-group-slug state))))
         (proj-name (if (and (stringp root) (not (string-empty-p root)))
                        (file-name-nondirectory (directory-file-name root))
                      "~")))
    (cond
     ((eq context-navigator-view--mode 'groups)
      (format "[%s]" proj-name))
     (t
      (if (and (stringp group) (not (string-empty-p group)))
          (format "[%s: %s]" proj-name group)
        (format "[%s]" proj-name))))))

(defun context-navigator-view--state-items ()
  "Get items from core state."
  (let* ((st (ignore-errors (context-navigator--state-get))))
    (and st (context-navigator-state-items st))))

;; Helpers

(defun context-navigator-view--invalidate-render-caches (&optional also-headerline)
  "Invalidate render caches for the Navigator view buffer.
When ALSO-HEADERLINE is non-nil, also reset header-line cache locals."
  (setq-local context-navigator-render--last-hash nil)
  (setq-local context-navigator-view--last-render-key nil)
  (when also-headerline
    (setq-local context-navigator-controls--cache-key nil)
    (setq-local context-navigator-controls--cache-str nil)))


(defun context-navigator-view--invalidate-openable ()
  "Invalidate cached openable counters (delegates to counters module)."
  (ignore-errors (context-navigator-view-counters-invalidate)))

(defun context-navigator-view--openable-count-refresh ()
  "Delegate refresh to counters module."
  (ignore-errors (context-navigator-view-counters-refresh-openable)))

(defun context-navigator-view--openable-count-get ()
  "Return cached openable count from counters module (COUNT . PLUS)."
  (ignore-errors (context-navigator-view-counters-get-openable)))

(defun context-navigator-view--collect-closable-buffers ()
  "Delegate closable-buffers collection to counters module."
  (ignore-errors (context-navigator-view-counters-collect-closable)))

;; Loading spinner helpers ----------------------------------------------------

(defun context-navigator-view--spinner-start ()
  "Start or restart the lightweight loading spinner timer.
Degrades to a static indicator when timer slippage exceeds threshold."
  (ignore-errors (context-navigator-view-spinner-start)))

(defun context-navigator-view--spinner-stop ()
  "Stop the loading spinner timer and reset index."
  (ignore-errors (context-navigator-view-spinner-stop)))


;; Entry point

(defun context-navigator-view--render-loading (state header total-width)
  "Render a lightweight loading/preloader view into the sidebar buffer.

Header is displayed in the header-line now; do not render a long title or
separator at the top of the buffer. Keep a small centered spinner/loading line."
  (let* ((hl "") ;; no header in buffer
         (sep "") ;; no separator
         (pct (when (and (consp context-navigator-view--load-progress)
                         (numberp (car context-navigator-view--load-progress))
                         (numberp (cdr context-navigator-view--load-progress))
                         (> (cdr context-navigator-view--load-progress) 0))
                (floor (* 100.0 (/ (float (car context-navigator-view--load-progress))
                                   (max 1 (cdr context-navigator-view--load-progress)))))))
         (ch (or (ignore-errors (context-navigator-view-spinner-current-frame pct)) ""))
         (label (if pct
                    (let* ((pos (and (consp context-navigator-view--load-progress)
                                     (car context-navigator-view--load-progress)))
                           (tot (and (consp context-navigator-view--load-progress)
                                     (cdr context-navigator-view--load-progress)))
                           (suffix (if (and (integerp pos) (integerp tot) (> tot 0))
                                       (format " (%d/%d)" pos tot)
                                     "")))
                      (format "%s%d%%%s" ch pct suffix))
                  (context-navigator-i18n :loading)))
         (spin-w (max 0 (string-width label)))
         (left-pad (max 0 (floor (/ (max 0 (- total-width spin-w)) 2))))
         (spin-line (concat (make-string left-pad ? ) label))
         (sline (propertize (concat " " spin-line) 'face 'shadow))
         (lines (list "" "" "" sline "")))
    (setq context-navigator-view--last-lines lines
          context-navigator-view--header header)
    (context-navigator-render-apply-to-buffer (current-buffer) lines)
    lines))

(defun context-navigator-view--render ()
  "Render current view (items or groups) into the sidebar buffer.

Uses a composite render key to skip full rendering when nothing relevant changed.
Key components:
 - model generation
 - current view mode (items/groups)
 - sidebar width (total)
 - gptel keys hash
 - cached openable count / soft-plus marker
 - header string (display)

When the key equals `context-navigator-view--last-render-key' the function
returns without rebuilding buffer contents.

Optimization: if the core state indicates loading in progress we render a
very small, cheap preloader view immediately (no icons, no sorting, no file
checks) so project switching feels responsive while the data loads in the
background."
  (catch 'context-navigator-view--render
    (condition-case err
        (let* ((state (context-navigator--state-get))
               (header (context-navigator-view--header state))
               (win (get-buffer-window (current-buffer) 'visible))
               (total (or (and win (window-body-width win))
                          (and (boundp 'context-navigator-view-width)
                               (symbol-value 'context-navigator-view-width))
                          33))
               ;; Components for early-exit render key
               (gen (or (and (context-navigator-state-p state)
                             (context-navigator-state-generation state))
                        0))
               (mode context-navigator-view--mode)
               ;; Stable fingerprint of gptel keys list
               (gptel-hash (sxhash-equal context-navigator-view--gptel-keys))
               ;; Use cached openable count (may be nil) — normalize to integer and plus marker.
               (openable (or context-navigator-view--openable-count 0))
               (plus (and context-navigator-view--openable-plus t))
               (push-on (and (boundp 'context-navigator--push-to-gptel)
                             context-navigator--push-to-gptel))
               (auto-on (and (boundp 'context-navigator--auto-project-switch)
                             context-navigator--auto-project-switch))
               ;; Stabilize groups hash by sorting (slug . display) pairs to avoid render loops
               (groups-hash
                (and (eq mode 'groups)
                     (let* ((pairs (cl-loop for pl in (or context-navigator-view--groups '())
                                            collect (cons (plist-get pl :slug)
                                                          (plist-get pl :display))))
                            (sorted (cl-sort (copy-sequence pairs) #'string< :key #'car)))
                       (sxhash-equal sorted))))
               ;; Include filter state so live filter typing forces re-render
               (fmode context-navigator-view--filter-mode)
               (fquery (or context-navigator-view--filter-query ""))
               ;; Compose key (include session flags so toggles force a refresh)
               (key (list gen mode total gptel-hash openable plus header fmode fquery
                          push-on auto-on context-navigator-view--collapsed-p groups-hash)))
          (when (equal key context-navigator-view--last-render-key)
            (ignore-errors
              (context-navigator-debug :trace :ui "render: skip (same key) %S" key)))
          (unless (equal key context-navigator-view--last-render-key)
            (ignore-errors
              (context-navigator-debug :trace :ui "render: run key=%S mode=%s total=%s" key mode total))
            (setq context-navigator-view--last-render-key key)
            ;; Fast path: show minimal preloader when loading or when progress is reported by events.
            (when (or (and (context-navigator-state-p state)
                           (context-navigator-state-loading-p state))
                      context-navigator-view--load-progress)
              (ignore-errors
                (context-navigator-debug :trace :ui
                                         "render: preloader loading=%s progress=%s"
                                         (and (context-navigator-state-p state)
                                              (context-navigator-state-loading-p state))
                                         context-navigator-view--load-progress))
              (context-navigator-view--render-loading state header total)
              (throw 'context-navigator-view--render nil))
            (context-navigator-view-render-items state header total)
            ;; Refresh pinned title (posframe) after render
            (ignore-errors
              (when (fboundp 'context-navigator-title-refresh)
                (context-navigator-title-refresh)))))
      (error
       ;; Prevent render-time errors from cascading into endless redisplay loops
       (message "context-navigator: render error: %s" (error-message-string err))
       (setq context-navigator-view--last-render-key nil)
       (context-navigator-render-apply-to-buffer (current-buffer) (list "" "" "" "Render error" ""))
       nil))))

(defun context-navigator-view--render-if-visible ()
  "Render sidebar if its buffer is visible."
  (when-let* ((buf (get-buffer context-navigator-view--buffer-name))
              (win (get-buffer-window buf t)))
    (with-selected-window win
      (with-current-buffer buf
        (ignore-errors
          (context-navigator-debug :trace :ui
                                   "render-if-visible: win=%s sel=%s"
                                   win (eq (selected-window) win)))
        (context-navigator-view--render)))))

(defun context-navigator-view--schedule-render (&optional also-invalidate)
  "Debounced request to render the sidebar if visible.

When ALSO-INVALIDATE is non-nil, reset render and header-line caches to force update.
When nil, keep caches to minimize flicker — the render will still happen if the
model generation changed."
  (when context-navigator-view--diag-trace-on
    (let* ((now (float-time))
           (recent (and (plistp context-navigator-view--diag-last-event)
                        (numberp (plist-get context-navigator-view--diag-last-event :time))
                        (< (- now (plist-get context-navigator-view--diag-last-event :time))
                           (max 0.0 (or context-navigator-view-diagnostics-event-window 0.0)))))
           (payload (list :invalidate also-invalidate
                          :mode context-navigator-view--mode
                          :visible (when (get-buffer-window context-navigator-view--buffer-name t) t))))
      (when recent
        (setq payload
              (append payload
                      (list :after-event (plist-get context-navigator-view--diag-last-event :event)
                            :dt (- now (plist-get context-navigator-view--diag-last-event :time))
                            :arity (plist-get context-navigator-view--diag-last-event :arity)))))
      (when (and context-navigator-view-diagnostics-include-backtrace
                 (fboundp 'backtrace))
        (setq payload
              (append payload
                      (list :bt (context-navigator-view--diag-backtrace 6)))))
      (context-navigator-view--diag-log :schedule payload)))
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when also-invalidate
          ;; Reset caches only when explicitly requested
          (context-navigator-view--invalidate-render-caches t)))))
  (ignore-errors
    (context-navigator-debug :trace :ui "schedule-render also-invalidate=%s" also-invalidate))
  (context-navigator-events-debounce
   :sidebar-render 0.12
   #'context-navigator-view--render-if-visible))

(defun context-navigator-view--schedule-render-soft ()
  "Debounced render without forcing cache invalidation (minimal flicker)."
  (context-navigator-view--schedule-render nil))

;; Moved to context-navigator-view-actions.el (compat declarations for byte-compiler)
(declare-function context-navigator-view--at-item "context-navigator-view-actions" ())
(declare-function context-navigator-view--visit "context-navigator-view-actions" (preview))
(declare-function context-navigator-view-visit "context-navigator-view-actions" ())
(declare-function context-navigator-view-preview "context-navigator-view-actions" ())
(declare-function context-navigator-view-next-item "context-navigator-view-navigation" ())
(declare-function context-navigator-view-previous-item "context-navigator-view-navigation" ())

(defvar context-navigator-view--title-line-keymap
  (let ((m (make-sparse-keymap)))
    ;; Mouse click toggles collapse/expand
    (define-key m [mouse-1] #'context-navigator-view-toggle-collapse-immediate)
    ;; TAB on title behaves like in Magit: toggle collapse
    (define-key m (kbd "TAB")       #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "<tab>")     #'context-navigator-view-toggle-collapse-immediate)
    (define-key m [tab]             #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "C-i")       #'context-navigator-view-toggle-collapse-immediate)
    ;; RET on title also toggles collapse/expand
    (define-key m (kbd "RET")       #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "C-m")       #'context-navigator-view-toggle-collapse-immediate)
    (define-key m [return]          #'context-navigator-view-toggle-collapse-immediate)
    (define-key m (kbd "<return>")  #'context-navigator-view-toggle-collapse-immediate)
    m)
  "Keymap attached to the title line to support mouse/TAB/RET collapse/expand.")

(defun context-navigator-view--headerline-format ()
  "Return title string for Navigator header-line (project[: group]) plus filter segment.

Be robust: if the title compute returns nil/empty for any reason, fall back to a
cheap text-only header built from core state."
  (let* ((s (condition-case _err
                (and (fboundp 'context-navigator-title--compute)
                     (context-navigator-title--compute))
              (error nil)))
         (seg (ignore-errors
                (and (fboundp 'context-navigator-view--filter-header-segment)
                     (context-navigator-view--filter-header-segment)))))
    ;; Fallback when title compute failed or returned an empty string.
    (unless (and (stringp s) (> (length s) 0))
      (let* ((st   (ignore-errors (context-navigator--state-get)))
             (root (and st (ignore-errors (context-navigator-state-last-project-root st))))
             (slug (and st (ignore-errors (context-navigator-state-current-group-slug st))))
             (proj (if (and (stringp root) (not (string-empty-p root)))
                       (file-name-nondirectory (directory-file-name root))
                     "~")))
        (setq s (if (and (stringp slug) (not (string-empty-p slug)))
                    (format "  [%s: %s]" proj slug)
                  (format "  [%s]" proj)))))
    (let ((title (or s "")))
      (if (and (stringp seg) (> (length seg) 0))
          (concat title "   " seg)
        title))))

(defvar context-navigator-view--group-line-keymap
  (let ((m (make-sparse-keymap)))
    ;; Mouse only; all keyboard keys come from the parent mode-map via keyspec.
    (define-key m [mouse-1] #'context-navigator-view-mouse-open-group)
    m)
  "Keymap attached to group lines.
Keyboard bindings are inherited from `context-navigator-view-mode-map'.")

(defvar context-navigator-view-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Keep only essential remaps here; all other bindings come from keyspec.
    (define-key m [remap delete-other-windows] #'context-navigator-delete-other-windows)
    (define-key m [remap indent-for-tab-command] #'context-navigator-view-tab-next)
    (define-key m [remap keyboard-quit] #'context-navigator-view--keyboard-quit-or-clear)
    ;; Safe defaults so core actions work even if keyspec hasn't been applied yet.
    ;; Keys from keyspec (when applied) will override these.
    ;; Undo/Redo come from keyspec (items/groups-split contexts)
    m)
  "Keymap for =context-navigator-view-mode'.
Only minimal remaps are defined here; all other bindings are applied from `context-navigator-keyspec'.")

;; Ensure group line keymap inherits major mode map so keyboard works on group lines
(when (and (keymapp context-navigator-view--group-line-keymap)
           (keymapp context-navigator-view-mode-map))
  (set-keymap-parent context-navigator-view--group-line-keymap context-navigator-view-mode-map))
(when (and (boundp 'context-navigator-view--title-line-keymap)
           (keymapp context-navigator-view--title-line-keymap)
           (keymapp context-navigator-view-mode-map))
  (set-keymap-parent context-navigator-view--title-line-keymap context-navigator-view-mode-map))

;; Apply centralized keyspec bindings (middle path)
(when (fboundp 'context-navigator-keys-apply-to)
  (context-navigator-keys-apply-to context-navigator-view-mode-map 'items))

(defun context-navigator-view--hl-line-range ()
  "Return region to highlight for the current line.

Highlight:
- item lines (have 'context-navigator-item)
- group lines (have 'context-navigator-group-slug)
- the \"..\" line (has 'context-navigator-groups-up)

Do not highlight purely decorative separators."
  (when (or (get-text-property (point) 'context-navigator-item)
            (get-text-property (point) 'context-navigator-group-slug))
    (cons (line-beginning-position)
          (min (point-max) (1+ (line-end-position))))))

(define-derived-mode context-navigator-view-mode special-mode "Context-Nav"
  "Major mode for context-navigator sidebar buffer."
  (buffer-disable-undo)
  (setq truncate-lines t
        cursor-type t)
  ;; Apply minimal modeline (safe; uses struct accessors and persist state)
  (when (fboundp 'context-navigator-modeline--apply)
    (context-navigator-modeline--apply (current-buffer)))
  ;; Header line: show title [project[: group]]
  (setq header-line-format '((:eval (context-navigator-view--headerline-format))))
  ;; Remap header-line face locally so Navigator header uses default background (no gray bar)
  (when context-navigator--headerline-face-cookie
    (ignore-errors (face-remap-remove-relative context-navigator--headerline-face-cookie))
    (setq context-navigator--headerline-face-cookie nil))
  (setq context-navigator--headerline-face-cookie
        (face-remap-add-relative 'header-line 'context-navigator-headerline '(:box nil)))
  ;; Используем стандартный hl-line (без собственных оверлеев)
  (hl-line-mode 1)
  ;; Track last cursor anchor (item key or "..") cheaply on every command; persist only on exit.
  (setq context-navigator-view--cursor-post-cmd-fn #'context-navigator-view--track-cursor-post-cmd)
  (add-hook 'post-command-hook context-navigator-view--cursor-post-cmd-fn nil t))

;;;###autoload
(defun context-navigator-view-open ()
  "Open the context-navigator sidebar on the left."
  (interactive)
  (let* ((buf (get-buffer-create context-navigator-view--buffer-name))
         (win (display-buffer-in-side-window buf
                                             (append
                                              context-navigator-view-window-params
                                              (list (cons 'window-width
                                                          (or (and (boundp 'context-navigator-view-width)
                                                                   (symbol-value 'context-navigator-view-width))
                                                              33)))))))
    ;; Mark sidebar window(s) immediately so splits can anchor correctly.
    (when (window-live-p win)
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (set-window-parameter w 'context-navigator-view 'sidebar))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (context-navigator-view-mode)
        ;; Ensure modeline toolbar is applied immediately even if hooks didn’t fire yet.
        (when (fboundp 'context-navigator-modeline--apply)
          (context-navigator-modeline--apply (current-buffer)))
        (setq-local buffer-read-only t)
        (context-navigator-view-events-install)
        ;; Ensure core state is available when opening lazily via use-package.
        ;; Best-effort: enable mode (installs hooks/subs) and trigger a refresh
        ;; so the view can render real data even when initialization was deferred.
        (ignore-errors (require 'context-navigator-core))
        (when (fboundp 'context-navigator-mode)
          (unless (bound-and-true-p context-navigator-mode)
            (ignore-errors (context-navigator-mode 1))))
        (when (fboundp 'context-navigator-refresh)
          (ignore-errors (context-navigator-refresh)))
        (context-navigator-view--render)
        ;; Auto-open Stats split by default (session-only)
        (when (and (boundp 'context-navigator-stats-split-default-visible)
                   context-navigator-stats-split-default-visible
                   (fboundp 'context-navigator-stats-split-visible-p)
                   (not (context-navigator-stats-split-visible-p)))
          (ignore-errors (context-navigator-stats-split-open)))))
    (when (window-live-p win)
      (select-window win))
    win))

;;;###autoload
(defun context-navigator-view-close ()
  "Close the context-navigator sidebar if visible."
  (interactive)
  (context-navigator-view-quit))

(defun context-navigator-view-quit ()
  "Close Navigator sidebar windows and teardown view-local resources."
  (interactive)
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    (when (buffer-live-p buf)
      ;; Remove buffer-local subscriptions/timers safely.
      (with-current-buffer buf
        (ignore-errors (context-navigator-view--save-items-cursor-state))
        ;; remove our local post-command hook (idempotent)
        (ignore-errors (when context-navigator-view--cursor-post-cmd-fn
                         (remove-hook 'post-command-hook context-navigator-view--cursor-post-cmd-fn t)))
        (ignore-errors
          (when (fboundp 'context-navigator-view-events-remove)
            (context-navigator-view-events-remove))))
      ;; Disable pinned title
      (ignore-errors
        (when (fboundp 'context-navigator-title-disable)
          (context-navigator-title-disable)))
      ;; Close Groups/Stats splits (if open) before deleting sidebar windows
      (ignore-errors
        (when (fboundp 'context-navigator-groups-split-close)
          (context-navigator-groups-split-close)))
      (ignore-errors
        (when (fboundp 'context-navigator-stats-split-close)
          (context-navigator-stats-split-close)))
      ;; Delete all windows that show the sidebar variant of this buffer.
      (dolist (w (get-buffer-window-list buf nil t))
        (when (and (window-live-p w)
                   (eq (window-parameter w 'context-navigator-view) 'sidebar))
          (delete-window w)))
      ))
  t)

;;;###autoload
(defun context-navigator-view-toggle ()
  "Toggle the context-navigator sidebar."
  (interactive)
  (if (get-buffer-window context-navigator-view--buffer-name nil)
      (context-navigator-view-close)
    (context-navigator-view-open)))

;; Helpers for group mode

(defun context-navigator-view--at-group ()
  "Return cons (SLUG . DISPLAY) for group at point, or nil."
  (let* ((slug (get-text-property (point) 'context-navigator-group-slug))
         (disp (get-text-property (point) 'context-navigator-group-display)))
    (when (and (stringp slug) (not (string-empty-p slug)))
      (cons slug (or disp slug)))))

(defun context-navigator-view--open-group-at-point ()
  "Switch to group at point and return t on success."
  (interactive)
  (when-let* ((cell (context-navigator-view--at-group))
              (slug (car cell)))
    (ignore-errors (context-navigator-group-switch slug))
    (setq context-navigator-view--mode 'items)
    ;; Ensure one-shot restore of saved cursor/scroll on the upcoming items render.
    (setq context-navigator-view--restore-once t)
    (context-navigator-view--schedule-render)
    t))

(defun context-navigator-view-mouse-open-group (event)
  "Open group at mouse EVENT position."
  (interactive "e")
  (mouse-set-point event)
  (context-navigator-view--open-group-at-point))

;; Actions moved to context-navigator-view-actions.el

;;; Dispatchers and commands

(defun context-navigator-view-toggle-dispatch ()
  "Toggle item selection."
  (interactive)
  (call-interactively 'context-navigator-view-toggle-enabled))

;;;###autoload
(defun context-navigator-view-toggle-multi-group-shim ()
  "Compatibility shim for transient: toggle current group selection in groups view."
  (interactive)
  (call-interactively 'context-navigator-view-group-toggle-select))

;;;###autoload
(defun context-navigator-view-show-groups ()
  "Open sidebar (if needed) and show the Groups split for current project/global."
  (interactive)
  (let ((buf (get-buffer context-navigator-view--buffer-name)))
    ;; Ensure sidebar is open
    (unless (and buf (get-buffer-window buf t))
      (ignore-errors (context-navigator-view-open))
      (setq buf (get-buffer context-navigator-view--buffer-name)))
    ;; Open/focus Groups split (Sidebar stays in items mode)
    (ignore-errors (context-navigator-groups-split-open))
    (when-let ((win (get-buffer-window buf t)))
      (select-window win))))

(require 'context-navigator-view-help)

(defun context-navigator-view-stats-toggle ()
  "Toggle the 5-line Stats split below the Navigator sidebar."
  (interactive)
  (when (fboundp 'context-navigator-stats-split-toggle)
    (context-navigator-stats-split-toggle)))

(provide 'context-navigator-view)
;;; context-navigator-view.el ends here
