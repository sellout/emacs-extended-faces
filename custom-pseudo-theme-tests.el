;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'buttercup)
(require 'custom-pseudo-theme)

;; TODO: Add more comprehensive tests. These basically just ensure that we have
;;       the dependencies we need.

(describe "custom-pseudo-theme"
  (describe "-set-faces"
    (it "can be called"
      (expect (custom-pseudo-theme-set-faces 'test-pseudo-theme
                '(default ((t (:family "Comic Sans" :height 110)))))
              :not :to-be-truthy)))

  (describe "-set-variables"
    (it "can be called"
      (expect (custom-pseudo-theme-set-variables 'test-pseudo-theme
                '(ispell-program-name "usr/bin/ispell"))
              :not :to-be-truthy)))

  (describe "-set-local-variables"
    (it "can be called"
      (expect (custom-pseudo-theme-set-local-variables 'test-pseudo-theme
                '(octave (inferior-octave-program "/usr/bin/octave")))))
    ;; TODO: Doesn’t currently test what it says. But we (require 'octave) to
    ;;       force setting of the local vars for that package, which exercises
    ;;       more of the code.
    (it "doesn’t force loading"
      (expect (require 'octave) :to-be-truthy))))

(provide 'custom-pseudo-theme-tests)
;;; custom-pseudo-theme-tests.el ends here
