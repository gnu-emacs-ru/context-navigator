;;; context-navigator-groups-split.el --- Groups split below Navigator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Show the list of groups in a split below the Navigator (sidebar or buffer-mode).
;; - Toggle with [h] or modeline "Groups" toggle
;; - Auto-focus current group when opening
;; - Navigation keys in the split: j/n/<down>, k/p/<up>
;; - Select with l/RET; close with q/h
;; - Height auto-fits to content up to a limit and not more than half of the Navigator window
;; - Auto-closes when Navigator window disappears

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'context-navigator-core)
(require 'context-navigator-events)
(require 'context-navigator-persist)
(require 'context-navigator-i18n)
(require 'context-navigator-keyspec)     ;; ensure keyspec is loaded so bindings apply
(require 'context-navigator-view-groups) ;; reuse body-lines rendering/keymap where possible

;; Optional cross-module helpers (guard with fboundp at callsites)
(declare-function context-navigator-stats-split--visible-window "context-navigator-stats-split" ())
(declare-function context-navigator-stats-split--fit-window "context-navigator-stats-split" (win navw))

;; View helpers used to refresh and focus items when closing the split
(declare-function context-navigator-view--schedule-render "context-navigator-view" (&optional also-invalidate))
(declare-function context-navigator-view--render-if-visible "context-navigator-view" ())

(defgroup context-navigator-groups-split nil
  "Split buffer for Navigator groups list."
  :group 'context-navigator)

(defcustom context-navigator-groups-split-height 10
  "Preferred height (maximum) for the Groups split window in lines.
The split auto-fits to content and will not exceed half of the Navigator window."
  :type 'integer :group 'context-navigator-groups-split)

(defcustom context-navigator-groups-split-buffer-name "*Context Navigator Groups*"
  "Name of the Groups split buffer."
  :type 'string :group 'context-navigator-groups-split)

(defvar context-navigator-groups-split--subs nil
  "Event subscriptions active while the groups split is visible.")

(defvar context-navigator-groups-split--buffer nil
  "Groups split buffer object.")

(defvar context-navigator-groups-split--wcch-on nil
  "When non-nil, auto-close watcher is installed.")

(defvar-local context-navigator-groups-split--focus-once nil
  "When non-nil, focus this group slug exactly once after the next render.")

;; Dedicated keymap for group lines in the Groups split buffer.
;; Parent is the split mode map so keyboard bindings resolve to the split context.
(defvar context-navigator-groups-split--line-keymap
  (let ((m (make-sparse-keymap)))
    ;; Mouse-only in the line keymap; keyboard comes from the parent mode map.
    (define-key m [mouse-1] #'context-navigator-groups-split-mouse-select)
    m)
  "Keymap attached to group lines in the Groups split buffer.")

(defun context-navigator-groups-split-mouse-select (event)
  "Mouse click handler to select group under EVENT in the Groups split."
  (interactive "e")
  (mouse-set-point event)
  (context-navigator-groups-split-select))

(defun context-navigator-groups-split--adopt-line-keymaps ()
  "Replace per-line text keymaps with the split-specific line keymap.
This ensures that keys resolve through `context-navigator-groups-split-mode-map',
not through the Navigator sidebar maps."
  (let ((m context-navigator-groups-split--line-keymap))
    (when (keymapp m)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((p (text-property-not-all (point) (point-max)
                                          'context-navigator-group-slug nil)))
            (if (not p)
                (goto-char (point-max))
              (let* ((beg (or (previous-single-property-change p 'context-navigator-group-slug nil (point-min))
                              (point-min)))
                     (end (or (next-single-property-change p 'context-navigator-group-slug nil (point-max))
                              (point-max))))
                ;; Override both 'keymap and 'local-map to ensure precedence.
                (add-text-properties beg end (list 'keymap m 'local-map m))
                (goto-char end)))))))))

;; Major mode for Groups split buffer so keyspec/which-key can target it
(defvar context-navigator-groups-split-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Undo/Redo in Groups split
    (define-key m (kbd "C-_") #'context-navigator-undo)
    (define-key m (kbd "M-_") #'context-navigator-redo)
    m)
  "Keymap for `context-navigator-groups-split-mode'.")

(define-derived-mode context-navigator-groups-split-mode special-mode "CN-GroupsSplit"
  "Mode for the bottom split showing groups list."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Apply centralized keyspec bindings to this map:
  ;; - global first (menu/help/etc.)
  ;; - groups-split after (overrides q/h/RET/etc.)
  (when (fboundp 'context-navigator-keys-apply-to)
    (context-navigator-keys-apply-to context-navigator-groups-split-mode-map 'global)
    (context-navigator-keys-apply-to context-navigator-groups-split-mode-map 'groups-split)))

(defun context-navigator-groups-split--ensure-buffer ()
  "Create or return the Groups split buffer and enable its mode."
  (let* ((name (if (and (boundp 'context-navigator-groups-split-buffer-name)
                        (stringp context-navigator-groups-split-buffer-name))
                   context-navigator-groups-split-buffer-name
                 "*Context Navigator Groups*"))
         (buf (or (and (buffer-live-p context-navigator-groups-split--buffer)
                       context-navigator-groups-split--buffer)
                  (get-buffer-create name))))
    (setq context-navigator-groups-split--buffer buf)
    (with-current-buffer buf
      (context-navigator-groups-split-mode)
      ;; Ensure our per-line keymap inherits from the split mode map (not the sidebar map).
      (when (and (boundp 'context-navigator-groups-split--line-keymap)
                 (keymapp context-navigator-groups-split--line-keymap))
        (set-keymap-parent context-navigator-groups-split--line-keymap
                           context-navigator-groups-split-mode-map)))
    buf))

(defun context-navigator-groups-split--visible-window ()
  "Return the window showing the Groups buffer, or nil."
  (catch 'hit
    (let ((buf (and (buffer-live-p context-navigator-groups-split--buffer)
                    context-navigator-groups-split--buffer)))
      (when buf
        (dolist (w (get-buffer-window-list buf nil t))
          (when (window-live-p w)
            (throw 'hit w)))))
    nil))

(defun context-navigator-groups-split-visible-p ()
  "Return non-nil when the Groups split is visible."
  (and (window-live-p (context-navigator-groups-split--visible-window)) t))

(defun context-navigator-groups-split--collect-groups ()
  "Return list of group plists for current root (like persist-list-groups)."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (root (and st (ignore-errors (context-navigator-state-last-project-root st)))))
    (ignore-errors (context-navigator-persist-list-groups root))))



(defun context-navigator-groups-split--notify-changed ()
  "Refresh Navigator headerline/modeline after split visibility changes."
  (let ((buf (get-buffer "*context-navigator*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local context-navigator-controls--cache-key nil)
        (setq-local context-navigator-controls--cache-str nil)
        (force-mode-line-update t)))))

(defun context-navigator-groups-split--refit-all ()
  "Refit all Navigator splits (Groups and Stats) to content with half-window cap."
  (let ((navw (context-navigator-groups-split--nav-window)))
    (when (window-live-p navw)
      (when-let ((gw (context-navigator-groups-split--visible-window)))
        (context-navigator-groups-split--fit-window gw navw))
      (when (and (fboundp 'context-navigator-stats-split--visible-window)
                 (fboundp 'context-navigator-stats-split--fit-window))
        (let ((sw (ignore-errors (context-navigator-stats-split--visible-window))))
          (when (window-live-p sw)
            (ignore-errors (context-navigator-stats-split--fit-window sw navw))))))))

(defun context-navigator-groups-split--target-height (nav-win)
  "Compute preferred groups split height under NAV-WIN."
  (let* ((max-pref (max 1 (or context-navigator-groups-split-height 10)))
         (half (max 1 (floor (/ (window-total-height nav-win) 2))))
         (desired (min max-pref half)))
    desired))

(defvar context-navigator-groups-split--refit-pending nil)

(defun context-navigator-groups-split--schedule-refit ()
  "Debounce refit of all Navigator splits after window layout changes."
  (unless context-navigator-groups-split--refit-pending
    (setq context-navigator-groups-split--refit-pending t)
    (run-at-time 0.02 nil
                 (lambda ()
                   (setq context-navigator-groups-split--refit-pending nil)
                   (ignore-errors (context-navigator-groups-split--refit-all))))))

(defun context-navigator-groups-split--maybe-autoclose ()
  "Auto-close groups split when Navigator window disappears.
Also schedule a refit of all splits to their content after any window layout change."
  (let ((gw (context-navigator-groups-split--visible-window))
        (nw (context-navigator-groups-split--nav-window)))
    (when (and (window-live-p gw) (not (window-live-p nw)))
      (ignore-errors (context-navigator-groups-split-close))))
  (context-navigator-groups-split--schedule-refit))

(defun context-navigator-groups-split--install-subs ()
  "Install event subscriptions while groups split is visible."
  (unless context-navigator-groups-split--subs
    (setq context-navigator-groups-split--subs
          (list
           (context-navigator-events-subscribe
            :groups-list-updated
            (lambda (&rest _)
              (when (context-navigator-groups-split-visible-p)
                (context-navigator-groups-split--render))))
           (context-navigator-events-subscribe
            :project-switch
            (lambda (&rest _)
              (when (context-navigator-groups-split-visible-p)
                (context-navigator-groups-split--render))))
           (context-navigator-events-subscribe
            :group-selection-changed
            (lambda (&rest _)
              (when (context-navigator-groups-split-visible-p)
                (context-navigator-groups-split--render))))
           ;; Обновлять подсветку текущей группы при переключении
           (context-navigator-events-subscribe
            :group-switch-start
            (lambda (&rest _)
              (when (context-navigator-groups-split-visible-p)
                (context-navigator-groups-split--render))))
           (context-navigator-events-subscribe
            :group-switch-done
            (lambda (&rest _)
              (when (context-navigator-groups-split-visible-p)
                (context-navigator-groups-split--render))))))))

(defun context-navigator-groups-split--remove-subs ()
  "Remove previously installed subscriptions."
  (when context-navigator-groups-split--subs
    (mapc #'context-navigator-events-unsubscribe context-navigator-groups-split--subs)
    (setq context-navigator-groups-split--subs nil)))

;;;###autoload
(defun context-navigator-groups-split-open ()
  "Open the Groups split below the Navigator (sidebar or buffer-mode)."
  (interactive)
  (let ((navw (context-navigator-groups-split--nav-window)))
    (when (window-live-p navw)
      (let* ((inhibit-redisplay t)
             (window-combination-resize t)
             (existing (context-navigator-groups-split--visible-window)))
        (if (window-live-p existing)
            (progn
              ;; Fast path when already visible: avoid re-creating/reassigning buffers and reselection.
              (context-navigator-groups-split--install-subs)
              (unless context-navigator-groups-split--wcch-on
                (add-hook 'window-configuration-change-hook
                          #'context-navigator-groups-split--maybe-autoclose)
                (setq context-navigator-groups-split--wcch-on t))
              (context-navigator-groups-split--render)
              (context-navigator-groups-split--fit-window existing navw)
              (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-groups-split--refit-all))))
              (context-navigator-groups-split--notify-changed)
              existing)
          (let* ((buf (context-navigator-groups-split--ensure-buffer))
                 (target-height (context-navigator-groups-split--target-height navw))
                 (kind (window-parameter navw 'context-navigator-view))
                 (win (if (eq kind 'sidebar)
                          ;; Under sidebar: use side-window in same side
                          (let* ((side (or (window-parameter navw 'window-side) 'left))
                                 (params (list (cons 'side side)
                                               (cons 'slot 2)
                                               (cons 'window-height target-height))))
                            (display-buffer-in-side-window buf params))
                        ;; Buffer-mode: try to split; fallback to bottom side-window
                        (or (condition-case _err
                                (split-window navw (- target-height) 'below)
                              (error nil))
                            (with-selected-window navw
                              (display-buffer-in-side-window
                               buf `((side . bottom)
                                     (slot . 0)
                                     (window-height . ,target-height))))))))
            (when (window-live-p win)
              (set-window-buffer win buf)
              (set-window-parameter win 'context-navigator-groups t)
              (set-window-dedicated-p win t)
              (context-navigator-groups-split--install-subs)
              (unless context-navigator-groups-split--wcch-on
                (add-hook 'window-configuration-change-hook
                          #'context-navigator-groups-split--maybe-autoclose)
                (setq context-navigator-groups-split--wcch-on t))
              (context-navigator-groups-split--render)
              ;; Fit height after rendering so it reflects actual content.
              (context-navigator-groups-split--fit-window win navw)
              ;; Refit once more on next tick to allow the side-window layout to settle.
              (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-groups-split--refit-all))))
              (context-navigator-groups-split--notify-changed)
              (select-window win)
              win)))))))

;;;###autoload
(defun context-navigator-groups-split-close ()
  "Close the Groups split and remove subscriptions.
Always return focus to the items list in the Navigator."
  (interactive)
  (when-let* ((win (context-navigator-groups-split--visible-window)))
    (when (window-live-p win)
      (delete-window win)))
  (context-navigator-groups-split--remove-subs)
  (when context-navigator-groups-split--wcch-on
    (remove-hook 'window-configuration-change-hook
                 #'context-navigator-groups-split--maybe-autoclose)
    (setq context-navigator-groups-split--wcch-on nil))
  (context-navigator-groups-split--notify-changed)
  ;; Also refit after layout settles.
  (run-at-time 0 nil (lambda () (ignore-errors (context-navigator-groups-split--refit-all))))
  ;; Ensure Navigator is in items mode and focused with point on an item line
  (let ((navb (get-buffer "*context-navigator*"))
        (navw (context-navigator-groups-split--nav-window)))
    (when (buffer-live-p navb)
      (with-current-buffer navb
        (setq context-navigator-view--mode 'items)
        (setq context-navigator-view--restore-once t)
        (when (fboundp 'context-navigator-view--render-if-visible)
          (context-navigator-view--render-if-visible))))
    (when (window-live-p navw)
      (select-window navw)
      (when (buffer-live-p navb)
        (with-current-buffer navb
          (let ((pos (text-property-not-all (point-min) (point-max)
                                            'context-navigator-item nil)))
            (when pos
              (goto-char pos)
              (beginning-of-line)))))))
  t)

;;;###autoload
(defun context-navigator-groups-split-toggle ()
  "Context-aware toggle for the Groups split:
- When invoked from the Groups split, close it.
- When invoked from elsewhere (e.g., Navigator items), open if closed, otherwise focus it."
  (interactive)
  (if (eq major-mode 'context-navigator-groups-split-mode)
      (context-navigator-groups-split-close)
    (if (context-navigator-groups-split-visible-p)
        (let ((win (context-navigator-groups-split--visible-window)))
          (when (window-live-p win) (select-window win))
          win)
      (context-navigator-groups-split-open))))

(defun context-navigator-groups-split--focus-navigator ()
  "Move focus back to Navigator window if present."
  (when-let ((navw (context-navigator-groups-split--nav-window)))
    (select-window navw)))

;;;###autoload
(defun context-navigator-groups-split-select ()
  "Open the group at point; keep the split open and focus Navigator."
  (interactive)
  (let ((slug (get-text-property (point) 'context-navigator-group-slug)))
    (if (and (stringp slug) (not (string-empty-p slug)))
        (progn
          ;; Переключаем активную группу (события загрузки пойдут из core)
          (ignore-errors (context-navigator-group-switch slug))
          ;; Явно переключаем основное окно в режим items и перерисовываем
          (let ((nav-buf (get-buffer "*context-navigator*")))
            (when (buffer-live-p nav-buf)
              (with-current-buffer nav-buf
                (setq context-navigator-view--mode 'items)
                (setq context-navigator-view--restore-once t)
                (when (fboundp 'context-navigator-view--schedule-render)
                  (context-navigator-view--schedule-render)))))
          ;; Оставить сплит открытым, вернуть фокус в Navigator
          (context-navigator-groups-split--focus-navigator))
      (message "%s" (context-navigator-i18n :no-group-at-point)))))

;; Improve nav window detection: prefer any window showing the main Navigator buffer.
(defun context-navigator-groups-split--nav-window ()
  "Return Navigator window (sidebar or buffer-mode) on any frame."
  (catch 'hit
    (let ((nbuf (get-buffer "*context-navigator*")))
      (when (buffer-live-p nbuf)
        (dolist (w (get-buffer-window-list nbuf nil t))
          (when (window-live-p w)
            (throw 'hit w)))))
    nil))

(defun context-navigator-groups-split--fit-window (win navw)
  "Fit WIN to buffer with constraints relative to NAVW.
Respects manual enlargements when possible: only grows up to minimal desired height,
does not shrink if the user made the window taller."
  (when (and (window-live-p win) (window-live-p navw))
    (let* ((max-pref (max 1 (or context-navigator-groups-split-height 10)))
           (half     (max 1 (floor (/ (window-total-height navw) 2))))
           ;; Реальная высота контента буфера (в строках)
           (content  (with-current-buffer (window-buffer win)
                       (max 1 (count-lines (point-min) (point-max)))))
           (margin   (max 0 (or (and (boundp 'context-navigator-splits-extra-margin-lines)
                                     context-navigator-splits-extra-margin-lines)
                                0)))
           (desired  (min (+ content margin) (min max-pref half)))
           (respect  (if (boundp 'context-navigator-splits-respect-manual-resize)
                         context-navigator-splits-respect-manual-resize
                       t))
           (cur (window-total-height win))
           (window-combination-resize t))
      ;; Не фиксируем высоту — оставляем возможность ручного ресайза.
      (set-window-parameter win 'window-size-fixed nil)
      (set-window-parameter win 'window-preserved-size nil)
      ;; Только увеличиваем до минимально достаточной высоты (или всегда подгоняем, если respect=nil).
      (when (or (not respect) (< cur desired))
        (fit-window-to-buffer win desired 1))
      ;; Стабилизируем высоту для авто-ребалансировки, но не запрещаем ручной ресайз.
      (set-window-parameter win 'window-preserved-size (cons t (window-total-height win))))))


(defun context-navigator-groups-split--render ()
  "Render groups list lines into the split buffer, focus current group, and fit height."
  (when-let* ((win (context-navigator-groups-split--visible-window)))
    (let* ((buf (window-buffer win))
           (st (ignore-errors (context-navigator--state-get)))
           (slug (and st (ignore-errors (context-navigator-state-current-group-slug st))))
           (groups (or (context-navigator-groups-split--collect-groups) '())))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq-local context-navigator-view--groups groups)
          (let* ((lines (ignore-errors (context-navigator-view-groups-body-lines st)))
                 (help  (ignore-errors (context-navigator-view--groups-help-lines (window-body-width win))))
                 (all   (append (or lines '()) (or help '()))))
            ;; Trim trailing empty lines
            (while (and all (stringp (car (last all))) (string-empty-p (car (last all))))
              (setq all (butlast all)))
            ;; Insert lines without a trailing newline to avoid an extra blank row
            (let ((n (length all)) (i 0))
              (dolist (ln all)
                (insert (or ln ""))
                (setq i (1+ i))
                (when (< i n) (insert "\n"))))
            ;; Replace per-line keymaps so keys are resolved in the split context
            (context-navigator-groups-split--adopt-line-keymaps))
          ;; Preserve point: prefer one-shot focus; else keep current line's slug; else current group.
          (let* ((keep (save-excursion
                         (let ((p (point)))
                           (get-text-property p 'context-navigator-group-slug))))
                 (want (or context-navigator-groups-split--focus-once keep slug)))
            (setq context-navigator-groups-split--focus-once nil)
            (when (stringp want)
              (goto-char (point-min))
              (let (pos)
                (while (and (not pos) (< (point) (point-max)))
                  (let* ((p (point))
                         (s (get-text-property p 'context-navigator-group-slug)))
                    (when (and (stringp s) (string= s want))
                      (setq pos p)))
                  (goto-char (or (next-single-char-property-change (point) 'context-navigator-group-slug nil (point-max))
                                 (point-max))))
                (when pos
                  (goto-char pos)
                  (beginning-of-line)
                  (set-window-point win (point))))))
          ;; Fit height after rendering
          (when-let ((navw (context-navigator-groups-split--nav-window)))
            (context-navigator-groups-split--fit-window win navw)))))))

(provide 'context-navigator-groups-split)
;;; context-navigator-groups-split.el ends here
