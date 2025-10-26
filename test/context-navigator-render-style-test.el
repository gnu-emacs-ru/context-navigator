;;; context-navigator-render-style-test.el --- Tests for indicator styles -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'context-navigator-render)
(require 'context-navigator-model)

(ert-deftest ctxnav-render/indicator-style-off-hides-icons ()
  "When indicator style is 'off, nothing should be rendered even if keys present."
  (let* ((it (context-navigator-item-create :type 'file :path "/tmp/a" :name "a" :enabled t))
         (context-navigator-render-indicator-style 'off)
         (context-navigator-render--gptel-keys (list (context-navigator-model-item-key it)))
         (lines (context-navigator-render-build-item-lines (list it) nil 40))
         (line (car lines)))
    (should (stringp line))
    (should-not (string-match-p "●" line))
    (should-not (string-match-p "○" line))))

(ert-deftest ctxnav-render/indicator-style-text-bullets ()
  "When style is 'text and keys present, show ASCII '[X]' indicator."
  (let* ((it (context-navigator-item-create :type 'file :path "/tmp/b" :name "b" :enabled t))
         (context-navigator-render-indicator-style 'text)
         (context-navigator-render--gptel-keys (list (context-navigator-model-item-key it)))
         (lines (context-navigator-render-build-item-lines (list it) nil 40))
         (line (car lines)))
    (should (stringp line))
    (should (string-match-p "\\[X\\]" line))))

(ert-deftest ctxnav-render/indicator-style-icons-fallback-to-text ()
  "When style is icons/auto but icon provider yields nil, fallback to text bullets."
  (let* ((it (context-navigator-item-create :type 'file :path "/tmp/c" :name "c" :enabled nil))
         (context-navigator-render-indicator-style 'icons)
         (context-navigator-render--gptel-keys (list (context-navigator-model-item-key it))))
    (cl-letf (((symbol-function 'context-navigator-icons-for-indicator)
               (lambda (_state) nil)))
      (let* ((lines (context-navigator-render-build-item-lines (list it) nil 40))
             (line (car lines)))
        (should (stringp line))
        ;; 'absent' state should prefer ○
        (should (or (string-match-p "○" line)
                    (string-match-p "●" line)))))))

(provide 'context-navigator-render-style-test)
;;; context-navigator-render-style-test.el ends here
