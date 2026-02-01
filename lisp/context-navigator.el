;;; context-navigator.el --- Umbrella entry for Context Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Author: Peter Kosov <11111000000@email.com>
;; Maintainer: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/context-navigator
;; Keywords: convenience, tools
;; Version: 1.3.0
;; Package-Requires: ((emacs "29.1") (transient "0.3.0"))

;;; Commentary:
;; This is a small, clear entry point (umbrella) that exposes public commands
;; and defers loading to the respective modules via autoload.
;;
;; Main modules:
;; - context-navigator-core      — core state, commands, wiring
;; - context-navigator-events    — event bus and debouncer
;; - context-navigator-fp        — functional helpers
;; - context-navigator-model     — pure model (items, diff)
;; - context-navigator-persist   — v3 persistence layer (save/load async)
;; - context-navigator-gptel-bridge — gptel adapter (pull/apply, change advices)
;; - context-navigator-project   — project detection and :project-switch events
;; - context-navigator-render    — pure render helpers
;; - context-navigator-icons     — optional icon provider
;; - context-navigator-view   — sidebar UI (side window)

;;; Code:

(require 'context-navigator-compat)

;; Public commands from core (autoloaded)
;;;###autoload
(autoload 'context-navigator-mode "context-navigator-core" "Global minor mode for Context Navigator." t)
;;;###autoload
(autoload 'context-navigator-refresh "context-navigator-core" "Recompute indices and publish a refresh event." t)
;;;###autoload
(autoload 'context-navigator-context-load "context-navigator-core" "Load context for project/global (async)." t)
;;;###autoload
(autoload 'context-navigator-context-save "context-navigator-core" "Save current context to file." t)
;;;###autoload
(autoload 'context-navigator-context-unload "context-navigator-core" "Unload/clear context and switch to global." t)
;;;###autoload
(autoload 'context-navigator-groups-open "context-navigator-groups" "Open groups list (publish event for sidebar)." t)
;;;###autoload
(autoload 'context-navigator-group-switch "context-navigator-groups" "Switch active group." t)
;;;###autoload
(autoload 'context-navigator-group-create "context-navigator-groups" "Create a new group." t)
;;;###autoload
(autoload 'context-navigator-group-rename "context-navigator-groups" "Rename a group." t)
;;;###autoload
(autoload 'context-navigator-group-delete "context-navigator-groups" "Delete a group." t)
;;;###autoload
(autoload 'context-navigator-group-duplicate "context-navigator-groups" "Duplicate a group." t)
;;;###autoload
(autoload 'context-navigator-group-edit-description "context-navigator-groups" "Edit description for a group." t)

;; Sidebar entry points (autoloaded)
;;;###autoload
(autoload 'context-navigator-view-open "context-navigator-view" "Open the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-view-close "context-navigator-view" "Close the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-view-quit "context-navigator-view" "Close the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-view-toggle "context-navigator-view" "Toggle the sidebar window." t)
;;;###autoload
(autoload 'context-navigator-view-show-groups "context-navigator-view" "Open the sidebar and show groups list." t)

;;; Groups split (autoloads)
;;;###autoload
(autoload 'context-navigator-groups-split-open "context-navigator-groups-split" "Open Groups split below Navigator." t)
;;;###autoload
(autoload 'context-navigator-groups-split-close "context-navigator-groups-split" "Close Groups split." t)
;;;###autoload
(autoload 'context-navigator-groups-split-toggle "context-navigator-groups-split" "Toggle Groups split." t)
;;;###autoload
(autoload 'context-navigator-groups-split-visible-p "context-navigator-groups-split" "Return non-nil if Groups split is visible." nil)

;;; NOTE:
;;; The view functionality is split across multiple files (dispatch/actions/navigation/items/groups/etc).
;;; To make the sidebar usable when loaded lazily, expose common view entry points here via autoload.
;;; This helps callers (keymaps/transient) to trigger loading of their modules on demand.

;;;###autoload
(autoload 'context-navigator-view-activate "context-navigator-view-dispatch" "Activate entry at point in the sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-preview "context-navigator-view-actions" "Preview entry in other window." t)
;;;###autoload
(autoload 'context-navigator-view-next-item "context-navigator-view-navigation" "Select next item in sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-previous-item "context-navigator-view-navigation" "Select previous item in sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-toggle-enabled "context-navigator-view-actions" "Toggle inclusion/enabled flag for item at point." t)
;;;###autoload
(autoload 'context-navigator-view-toggle-gptel "context-navigator-view-actions" "Toggle gptel membership for item at point." t)
;;;###autoload
(autoload 'context-navigator-view-delete-dispatch "context-navigator-view-dispatch" "Delete item or group at point." t)
;;;###autoload
(autoload 'context-navigator-view-refresh-dispatch "context-navigator-view-dispatch" "Refresh items/groups in the sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-go-up "context-navigator-view-dispatch" "Go up to groups view from items." t)

;;; Group management actions (groups view)
;;;###autoload
(autoload 'context-navigator-view-group-create "context-navigator-view-dispatch" "Create a new group from the sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-group-rename "context-navigator-view-dispatch" "Rename a group from the sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-group-duplicate "context-navigator-view-dispatch" "Duplicate a group from the sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-group-edit-description "context-navigator-view-dispatch" "Edit a group's description from the sidebar." t)
;;;###autoload
(autoload 'context-navigator-view-toggle-multi-group "context-navigator-view-dispatch" "Toggle per-project multi-group mode (:multi in state.el)." t)

;;; Footer / batch actions
;;;###autoload
(autoload 'context-navigator-view-open-all-buffers "context-navigator-view-actions" "Open all context buffers in background." t)
;;;###autoload
(autoload 'context-navigator-view-close-all-buffers "context-navigator-view-actions" "Close all context buffers." t)
;;;###autoload
(autoload 'context-navigator-view-clear-group "context-navigator-view-actions" "Clear the current group's items." t)
;;;###autoload
(autoload 'context-navigator-view-clear-gptel "context-navigator-view-actions" "Clear gptel and disable all items." t)
;;;###autoload
(autoload 'context-navigator-view-enable-all-gptel "context-navigator-view-actions" "Enable all items and push to gptel." t)
;;;###autoload
(autoload 'context-navigator-view-disable-all-gptel "context-navigator-view-actions" "Disable all items in the current group and clear gptel." t)
;;;###autoload
(autoload 'context-navigator-view-toggle-all-gptel "context-navigator-view-actions" "Toggle all items in gptel (enable/clear)." t)
;;;###autoload
(autoload 'context-navigator-view-toggle-push "context-navigator-view-actions" "Toggle push→gptel session flag." t)
;;;###autoload
(autoload 'context-navigator-view-toggle-auto-project "context-navigator-view-actions" "Toggle auto-project session flag." t)
;;;###autoload
(autoload 'context-navigator-view-push-now "context-navigator-view-actions" "Push current items to gptel now." t)
;;;###autoload
(autoload 'context-navigator-view-razor-run "context-navigator-view-actions" "Run Occam/razor (LLM) against current org buffer." t)

;;; Context blocks (Org)
;;;###autoload
(autoload 'context-navigator-context-block-insert "context-navigator-context-blocks" "Insert a context block representing the current group's enabled items." t)
;;;###autoload
(autoload 'context-navigator-context-block-apply-add "context-navigator-context-blocks" "If inside a context block, parse and add items to the current group." t)
;;;###autoload
(autoload 'context-navigator-context-block-apply-replace "context-navigator-context-blocks" "If inside a context block, parse and replace items in the current group." t)

;;; Help / menu
;;;###autoload
(autoload 'context-navigator-view-help "context-navigator-view-help" "Show help for Navigator view." t)
;;;###autoload
(autoload 'context-navigator-view-open-menu "context-navigator-view-help" "Open Navigator menu (transient or help fallback)." t)

;;; Transient entrypoints
;;;###autoload
(autoload 'context-navigator-transient "context-navigator-transient" "Open Context Navigator transient." t)
;;;###autoload
(autoload 'context-navigator-view-transient "context-navigator-transient" "Open Navigator view transient." t)

;;; Multifile view
;;;###autoload
(autoload 'context-navigator-multifile-open "context-navigator-multifile" "Open Context Multifile View." t)
;;;###autoload
(autoload 'context-navigator-multifile-close "context-navigator-multifile" "Close Context Multifile View." t)

;;; Core entrypoints (unchanged)
;;;###autoload
(autoload 'context-navigator-open "context-navigator-core" "Open Navigator in current display mode." t)
;;;###autoload
(autoload 'context-navigator-close "context-navigator-core" "Close Navigator in current display mode." t)
;;;###autoload
(autoload 'context-navigator-toggle "context-navigator-core" "Toggle Navigator in current display mode." t)
;;;###autoload
(autoload 'context-navigator-display-mode-toggle "context-navigator-core" "Toggle display mode (buffer/sidebar) and reopen." t)
;;;###autoload
(autoload 'context-navigator-restart "context-navigator-core" "Hot-restart Context Navigator (reload modules)." t)

;;;###autoload
(defun context-navigator-version ()
  "Return version string for Context Navigator."
  (interactive)
  (message "context-navigator — 1.3.0")
  "1.3.0")

;;;###autoload
(defun context-navigator-start ()
  "Enable the mode, refresh model, and open Navigator (buffer or sidebar) per current display mode.
Ensure session flags follow defaults. Initial project switch is handled by the mode when enabled."
  (interactive)
  (context-navigator-mode 1)     ;; autoloads core and triggers initial project switch when ON
  ;; Ensure session flags follow defaults on startup
  (setq context-navigator--auto-project-switch context-navigator-default-auto-project-switch)
  (setq context-navigator--push-to-gptel       context-navigator-default-push-to-gptel)
  (context-navigator-refresh)    ;; autoloads core if needed
  (ignore-errors (context-navigator-open)))

;;; Multifile view (autoloads)
;;;###autoload
(autoload 'context-navigator-multifile-open "context-navigator-multifile"
  "Open Context Multifile View." t)
;;;##
(autoload 'context-navigator-multifile-close "context-navigator-multifile"
  "Close Context Multifile View." t)

;; Optional which-key integration (auto-applies labels from keyspec)
(ignore-errors (require 'context-navigator-which-key nil t))

;;; Diagnostics helpers
;;;###autoload
(autoload 'context-navigator-diagnostics-clean-caches "context-navigator-diagnostics"
  "Delete Context Navigator .elc and .eln caches (safe) and report counts." t)
;;;###autoload
(autoload 'context-navigator-diagnostics-hard-restart "context-navigator-diagnostics"
  "Clean caches and hot-restart Context Navigator (unload/reload)." t)

(provide 'context-navigator)
;;; context-navigator.el ends here
