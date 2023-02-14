;;; custom-pseudo-theme.el --- Themes that aren’t enabled  -*- lexical-binding: t; -*-

;;; Commentary:

;; This creates themes with less boilerplate and doesn’t keep them in
;; ‘custom-enabled-themes’, so they don’t get accidentally disabled.

;; This is very useful for packages that want to configure _other_ packages for
;; you. E.g., ‘use-package’, which is where this code was initially stolen from.

;; The benefits of this approach is that variables (and faces) customized this
;; way appear as “THEMED” in Customize, rather than “CHANGED outside Customize”,
;; and the comments tell you which package customized it, rather than leaving
;; you to dig for where something got set.

;; As an Emacs user, you may also find it useful to use in your own config, as
;; you can group variables and have a reference back to where in your config you
;; customized it.

;;; Code:

(defun cst--set (set-fn pseudo-theme &rest args)
  "This is a utility for managing custom values “outside of” a theme."
  (unless (memq pseudo-theme custom-known-themes)
    (custom-declare-theme pseudo-theme
                          (custom-make-theme-feature pseudo-theme)))
  (prog1
      (apply set-fn pseudo-theme args)
    (enable-theme pseudo-theme)
    (setq custom-enabled-themes (remq pseudo-theme custom-enabled-themes))))

(defun cst-set-faces (pseudo-theme &rest args)
  "This is a utility for managing custom faces “outside of” a theme.
They variables are still treated as THEMED, but a PSEUDO-THEME doesn’t
appear in ‘custom-enabled-themes’. See ‘custom-theme-set-faces’ for the
structure of ARGS."
  (apply
   #'cst--set
   #'custom-theme-set-faces
   pseudo-theme
   (let ((comment-addendum (format "(Customized by %s.)" pseudo-theme)))
     (mapcar
      (lambda (def)
        (let ((face (nth 0 def))
              (spec (nth 1 def))
              (now (nth 2 def))
              (comment (nth 3 def)))
          (list face
                spec
                now
                ;; TODO: This produces the correct result, but
                ;;       Emacs drops the comment at some point.
                (if (stringp comment)
                    (concat comment " " comment-addendum)
                  comment-addendum))))
      args))))

;; Adapted from jwiegley/use-package#881 and jwiegley/use-package#899.
(defun cst-set-variables (pseudo-theme &rest args)
  "This is a utility for managing custom values “outside of” a theme.
They variables are still treated as THEMED, but a PSEUDO-THEME doesn’t
appear in ‘custom-enabled-themes’. See ‘custom-theme-set-variables’ for the
structure of ARGS."
  (apply
   #'cst--set
   #'custom-theme-set-variables
   pseudo-theme
   (let ((comment-addendum (format "(Customized by %s.)" pseudo-theme)))
     (mapcar
      (lambda (def)
        (let ((symbol (nth 0 def))
              (exp (nth 1 def))
              (now (nth 2 def))
              (request (nth 3 def))
              (comment (nth 4 def)))
          (list symbol
                exp
                now
                request
                ;; TODO: This produces the correct result, but
                ;;       Emacs drops the comment at some point.
                (if (stringp comment)
                    (concat comment " " comment-addendum)
                  comment-addendum))))
      args))))

(defun cst-set-local-variables (pseudo-theme &rest args)
  "Set the variables for PSEUDO-THEME _only_ in the local context.
Over a remote connection, they will have their previous values. This also
allows us to set global values first, then use this to override the local
ones, rather than having to set up a connection-local profile explicitly.
See ‘custom-theme-set-variables’ for the structure of ARGS.

\(fn PSEUDO-THEME (PACKAGE &rest ARGS)...)"
  (mapc (lambda (arg)
          (cl-destructuring-bind (package &rest args) arg
            (eval-after-load package
              `(progn
                 (let ((profile (intern (concat (symbol-name ',pseudo-theme)
                                                "-"
                                                (symbol-name ',package)))))
                   (connection-local-set-profile-variables
                    profile
                    (mapcar (lambda (arg)
                              (let ((sym (car arg)))
                                (cons sym (symbol-value sym))))
                            ',args))
                   (connection-local-set-profiles '() profile))
                 ;; NB: This can’t be done outside ‘eval-after-load’ because we
                 ;;     need to set up the connection-local variables from the
                 ;;     standard values before we change their values.
                 (apply #'cst-set-variables ',pseudo-theme ',args)))))
        args))

;; Local Variables:
;; read-symbol-shorthands: (("cst-" . "custom-pseudo-theme-"))
;; End:

(provide 'custom-pseudo-theme)
;;; custom-pseudo-theme.el ends here
