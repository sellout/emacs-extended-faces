;;; interim-faces.el --- Inject extended faces -*- lexical-binding: t; -*-

;;; Commentary:

;; This module integrates `extended-faces' with many different packages, to
;; provide more consistent theming across all of Emacs.
;;
;; Packages that are referenced here do not directly depend on extended-faces,
;; but as they begin to, they should be removed from here.

;;; Code:

(require 'extended-faces)

;;; built-in

;; There are a few different diff libraries. We try to cover the various bits of
;; all of them. Here is a rough equivalence table:
;;
"|      Diff       |   EDiff   |       Magit       |       SMerge (VC)        |
 +-----------------+-----------+-------------------+--------------------------+
 | -added          | -B        | -added, -their    | -upper, -other (-added)  |
 | -changed        | -Ancestor | -base             | -base (-changed)         |
 | -removed        | -A        | -removed, -our    | -lower, -mine (-removed) |
 | -changed-unspec | -C        | -lines            |                          |
 | -context        |           | -context          |                          |
 +-----------------+-----------+-------------------+--------------------------+
 |                 | -current- | -highlight        |                          |
 | -indicator-     |           | -diffstat-        |                          |
 | -refine-        | -fine-    |                   | -refined-                |
 |                 | -even-    |                   |                          |
 |                 | -odd-     |                   |                          |
 +-----------------+-----------+-------------------+--------------------------+
 | -file-header    |           | -file-heading     |                          |
 | -hunk-header    |           | -hunk-heading     |                          |
 |                 |           | -conflict-heading | -markers                 |
 | -index          |           |                   |                          |
 +-----------------+-----------+-------------------+--------------------------+"

(eval-after-load 'ediff
  '(default-mode-face 'fixed-pitch '(ediff-mode)))

(eval-after-load 'org
  '(progn
     (defeface org-agenda '((default (:inherit org-default)))
       ""
       :group 'org-faces)
     (defeface org-agenda-calendar '((default (:inherit org-agenda)))
       ""
       :group 'org-faces)))

;;; 3rd-party

(eval-after-load 'emacs-wiki-colors
  '(defeface emacs-wiki-header '((default (:inherit text-heading)))
     ""
     :group 'emacs-wiki-highlight)) ;; FIXME: check if this group is right

;;; Ensime is a little crazier, as it has a weird alist for semantic faces.
(eval-after-load 'ensime
  '(progn
     (defeface ensime-sem-high-var
       '((default (:inherit scala-font-lock:var-face)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-val '((default (:inherit font-lock-constant-face)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-var-field
       '((default (:inherit ensime-sem-high-var)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-val-field
       '((default (:inherit ensime-sem-high-val)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-function-call
       '((default (:inherit font-lock-function-name-face)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-operator
       '((default (:inherit font-lock-keyword-face)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-param '((default (:inherit ensime-sem-high-val)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-class '((default (:inherit font-lock-type-face)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-trait '((default (:inherit font-lock-type-face)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-object
       '((default (:inherit font-lock-module-face)))
       ""
       :group 'ensime-ui)
     (defeface ensime-sem-high-package
       '((default (:inherit font-lock-module-face)))
       ""
       :group 'ensime-ui)
     ;; (setq ensime-sem-high-faces
     ;;       '((var          . ensime-sem-high-var)
     ;;         (val          . ensime-sem-high-val)
     ;;         (varField     . ensime-sem-high-var-field)
     ;;         (valField     . ensime-sem-high-val-field)
     ;;         (functionCall . ensime-sem-high-function-call)
     ;;         (operator     . ensime-sem-high-operator)
     ;;         (param        . ensime-sem-high-param)
     ;;         (class        . ensime-sem-high-class)
     ;;         (trait        . ensime-sem-high-trait)
     ;;         (object       . ensime-sem-high-object)
     ;;         (package      . ensime-sem-high-package)))
     ))

;; FIXME doesn’t derive from prog-mode for some reason
(default-mode-face 'font-lock '(haskell-cabal-mode))

(default-mode-face 'pseudo-column '(helm-major-mode))

(defeface magit-diff-file-contents
  '((default (:inherit fixed-pitch)))
  "Used for the contents of the file being compared.
This inherits ‘fixed-pitch’ because columnar alignment in comparisons is
generally useful."
  :group 'magit-faces)

(eval-after-load 'magit
  '(progn
     (default-mode-face 'fixed-pitch
       '(magit-log-mode                 ; uses columns, but not enough
         magit-popup-mode
         magit-refs-mode))))

(eval-after-load 'scala-mode2
  '(defeface scala-font-lock:keyword-face
     '((default (:inherit font-lock-keyword-face)))
     "Font Lock mode face used for keywords."
     :group 'scala))

(eval-after-load 'slime
  '(progn
     (defeface sldb-default '((default (:inherit slime-default)))
       ""
       :group 'slime-debugger)
     (defeface slime-default '() ;; NB: font-lock?
       ""
       :group 'slime)
     (defeface slime-apropos-default '((default (:inherit slime-default)))
       ""
       :group 'slime)
     (defeface slime-inspector-default '((default (:inherit slime-default)))
       ""
       :group 'slime-inspector)
     (defeface slime-topline '((default (:inherit slime-default)))
       ""
       :group 'slime)))

(eval-after-load 'which-key
  '(defeface which-key-description-face '()
     "Used for the descriptions of key bindings."
     :group 'which-key))

(eval-after-load 'whitespace
  '(defeface whitespace-default '((default (:inherit warning)))
     ""
     :group 'whitespace))

;; Add us to ‘custom-theme-load-path’ so that ‘inheritance’ is available as a
;; theme.
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'interim-faces)
;;; interim-faces.el ends here
