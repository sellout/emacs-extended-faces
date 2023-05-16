;;; theme-kit.el --- Utilities for themes  -*- lexical-binding: t; -*-

;;; Commentary:

;; I generally group faces by package. However, since this theme is
;; auto-generated, it has been minimally edited (whitespace only) in order to
;; make it easy to compare to regenerations.

;; If this is auto-generated, why is it committed this way rather than as the
;; expression that generates it? Primarily because this has to be executed in a
;; pretty pristine environment – other code (for example, ‘extended-faces’) can
;; modify the “standard” values for the faces, which would cause this to be
;; generated with the incorrect values. By making it explicit, we can always
;; verify before any changes are made.

;;; Code:

(require 'cus-theme)

;; TODO: This should not involve the Customize interface at all, instead this
;;       function should produce a buffer containing the contents of the desired
;;       theme.
(defun tk-generate-theme (faces)
  "Create a buffer containing the current specs for the provided FACES."
  "Create a buffer containing the settings for the ‘default’ theme."
  (let ((custom-theme--listed-faces faces))
    (customize-create-theme)))

(defun tk-generate-basic-theme ()
  "Create a buffer containing the settings for the ‘basic’ theme.
The ‘basic’ theme is a subset of the ‘standard’ theme."
  (tk-generate-theme custom-theme--listed-faces))

(defun tk-generate-standard-theme ()
  "Create a buffer containing the settings for the ‘standard’ theme.
The ‘standard’ theme consists of the specs that are provided in the ‘defface’
forms for faces included with Emacs."
  (tk-generate-theme (face-list)))

(defun tk-advise-skip-recalc-variables (&rest variables)
  "Produce a function that prevents recalculating the given VARIABLES.
It is meant to be added to ‘:around’ advice on ‘custom-theme-recalc-variable’.
This is useful when you have a ‘defcustom’ whose ‘:set’ calls ‘enable-theme’,
which would otherwise try to recalc the variable, causing an infinite loop."
  (lambda (orig-fn variable)
    (unless (memq variable variables)
      (funcall orig-fn variable))))

;; Partially stolen from ‘custom-enabled-themes’ ‘:set’ argument.
(defun tk-reenable-themes (&optional themes)
  "Re-enables THEMES, which defaults to ‘custom-enabled-themes’."
  (interactive)
  (let (failures)
    (setq themes (delq 'user (delete-dups (or themes custom-enabled-themes))))
    ;; Call `enable-theme' or `load-theme' on each of THEMES.
    (dolist (theme (reverse themes))
      (condition-case nil
          (if (custom-theme-p theme)
              (enable-theme theme)
            (load-theme theme))
        (error (push theme failures)
               (setq themes (delq theme themes)))))
    (enable-theme 'user)
    (when failures
      (message "Failed to enable theme(s): %s"
               (mapconcat #'symbol-name failures ", ")))))

(cl-defun tk-update-background-mode
    (appearance &optional (frames (frame-list)))
  "Set the APPEARANCE of all FRAMES to either 'light or 'dark.
This is not specific to Solarized – it will update the appearance of any theme
that observes the background characteristic."
  (setq frame-background-mode appearance)
  (mapc #'frame-set-background-mode frames)
  ;; Supposedly #'frame-set-background-mode updates the faces, but it doesn’t
  ;; seem to actually., so re-enable all the themes.
  (tk-reenable-themes)
  ;; For some reason, ‘enable-theme’ (or maybe ‘solarized’?) is resetting the
  ;; ‘frame-background-mode’, so reset it here.
  (setq frame-background-mode appearance))

(defun tk-toggle-background-mode (&optional mode)
  "Toggle between 'light and 'dark background MODE.
If MODE is nil, it will switch to whichever mode is _not_ currently active. This
will update the appearance of any theme that observes the background
characteristic."
  (interactive)
  (let ((new-mode (or mode
                      (pcase frame-background-mode
                        ('dark 'light)
                        (_ 'dark)))))
    (tk-update-background-mode new-mode)))

(defvar tk-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'tk-toggle-background-mode)
    (define-key map (kbd "r") 'tk-reenable-themes)
    map)
  "Theme-related key bindings.
Use ‘global-set-key’ to define a prefix key for this (suggestion: “C–c t”).")

;; Local Variables:
;; read-symbol-shorthands: (("tk-" . "theme-kit-"))
;; End:

(provide 'theme-kit)
;;; theme-kit.el ends here
