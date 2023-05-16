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
  '(ef-default-mode-face 'fixed-pitch '(ediff-mode)))

(eval-after-load 'org
  '(progn
     (ef-defface org-agenda '((default (:inherit org-default)))
       ""
       :group 'org-faces)
     (ef-defface org-agenda-calendar '((default (:inherit org-agenda)))
       ""
       :group 'org-faces)))

;;; 3rd-party

(eval-after-load 'emacs-wiki-colors
  '(ef-defface emacs-wiki-header '((default (:inherit text-heading)))
     ""
     :group 'emacs-wiki-highlight)) ;; FIXME: check if this group is right

;;; Ensime is a little crazier, as it has a weird alist for semantic faces.
(eval-after-load 'ensime
  '(progn
     (ef-defface ensime-sem-high-var
         '((default (:inherit scala-font-lock:var-face)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-val
         '((default (:inherit font-lock-constant-face)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-var-field
         '((default (:inherit ensime-sem-high-var)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-val-field
         '((default (:inherit ensime-sem-high-val)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-function-call
         '((default (:inherit font-lock-function-name-face)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-operator
         '((default (:inherit font-lock-keyword-face)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-param
         '((default (:inherit ensime-sem-high-val)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-class
         '((default (:inherit font-lock-type-face)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-trait
         '((default (:inherit font-lock-type-face)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-object
         '((default (:inherit font-lock-module-face)))
       ""
       :group 'ensime-ui)
     (ef-defface ensime-sem-high-package
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
(ef-default-mode-face 'font-lock '(haskell-cabal-mode))

(add-hook 'helm-top-after-init-hook
          (lambda ()
            (with-current-buffer "*helm top*"
              (buffer-face-set 'pseudo-column))))

(eval-after-load 'magit
  '(progn
     (ef-default-mode-face 'fixed-pitch
       '(magit-popup-mode
         magit-refs-mode))))

(eval-after-load 'scala-mode2
  '(ef-defface scala-font-lock:keyword-face
       '((default (:inherit font-lock-keyword-face)))
     "Font Lock mode face used for keywords."
     :group 'scala))

(eval-after-load 'slime
  '(progn
     (ef-defface sldb-default '((default (:inherit slime-default)))
       ""
       :group 'slime-debugger)
     (ef-defface slime-default '() ;; NB: font-lock?
       ""
       :group 'slime)
     (ef-defface slime-apropos-default '((default (:inherit slime-default)))
       ""
       :group 'slime)
     (ef-defface slime-inspector-default '((default (:inherit slime-default)))
       ""
       :group 'slime-inspector)
     (ef-defface slime-topline '((default (:inherit slime-default)))
       ""
       :group 'slime)))

(eval-after-load 'which-key
  '(ef-defface which-key-description-face '()
     "Used for the descriptions of key bindings."
     :group 'which-key))

(eval-after-load 'whitespace
  '(ef-defface whitespace-default '((default (:inherit warning)))
     ""
     :group 'whitespace))

(provide 'interim-faces)

;; Local Variables:
;; read-symbol-shorthands: (("ef-" . "extended-faces-"))
;; End:

;;; interim-faces.el ends here
