(require 'extended-faces)

(defun package-faces (package &rest faces)
  (eval-after-load package
    (lambda ()
      (mapcar (lambda (face) (apply #'set-face-inheritance face)) faces))))

;;; built-in
(package-faces 'ansi-term
               '(term-color-black   black)
               '(term-color-red     red)
               '(term-color-green   green)
               '(term-color-yellow  yellow)
               '(term-color-blue    blue)
               '(term-color-magenta magenta)
               '(term-color-cyan    cyan)
               '(term-color-white   white))

(package-faces 'comint
               '(comint-highlight-prompt prompt))

(package-faces 'compilation
               '(compilation-info    success)
               '(compilation-warning warning))

(package-faces 'eshell
               ;; '(eshell-ls-archive (,@fg-magenta))
               ;; '(eshell-ls-backup (,@fg-yellow))
               ;; '(eshell-ls-clutter (,@fg-orange))
               '(eshell-ls-directory  fs-directory)
               '(eshell-ls-executable fs-executable)
               '(eshell-ls-missing    fs-broken-symlink)
               ;; '(eshell-ls-product (,@fg-yellow))
               ;; '(eshell-ls-readonly (,@fg-base1))
               ;; '(eshell-ls-special (,@fg-violet))
               '(eshell-ls-symlink    symlink)
               ;; '(eshell-ls-unreadable (,@fg-base00))
               '(eshell-prompt prompt))

(package-faces 'gnus
               '(gnus-cite-1  level-1)
               '(gnus-cite-2  level-2)
               '(gnus-cite-3  level-3)
               '(gnus-cite-4  level-4)
               '(gnus-cite-5  level-5)
               '(gnus-cite-6  level-6)
               '(gnus-cite-7  level-7)
               '(gnus-cite-8  level-8)
               '(gnus-cite-9  level-9)
               '(gnus-cite-10 level-10)
               '(gnus-cite-11 level-11))

(package-faces 'info
               '(info-title-1      (level-1 text-heading))
               '(info-title-2      (level-2 text-heading))
               '(info-title-3      (level-3 text-heading))
               '(info-title-4      (level-4 text-heading))
               '(info-xref         link)
               '(info-xref-visited link-visited))

(package-faces 'org-mode
               '(org-default default)
               '(org-level-1 level-1)
               '(org-level-2 level-2)
               '(org-level-3 level-3)
               '(org-level-4 level-4)
               '(org-level-5 level-5)
               '(org-level-6 level-6)
               '(org-level-7 level-7)
               '(org-level-8 level-8)
               '(org-link    link)
               '(org-warning warning))

(package-faces 'outline
               '(outline-1 level-1)
               '(outline-2 level-2)
               '(outline-3 level-3)
               '(outline-4 level-4)
               '(outline-5 level-5)
               '(outline-6 level-6)
               '(outline-7 level-7)
               '(outline-8 level-8))

(package-faces 'rcirc
               '(rcirc-bright-nick               highlight)
               '(rcirc-dim-nick                  shadow)
               '(rcirc-keyword                   highlight)
               ;; '(rcirc-my-nick (,@fg-blue))
               '(rcirc-nick-in-message           match)
               '(rcirc-nick-in-message-full-line highlight)
               ;; '(rcirc-other-nick (,@fg-green))
               '(rcirc-prompt                    prompt)
               '(rcirc-server                    alert-normal)
               '(rcirc-server-prefix             rcirc-server)
               ;; '(rcirc-timestamp (,@fg-base01))
               '(rcirc-track-keyword             alert-normal)
               '(rcirc-track-nick                alert-moderate)
               '(rcirc-url                       link))

(package-faces 'speedbar
               '(speedbar-directory-face fs-directory)
               '(speedbar-file-face      fs-file)
               '(speedbar-highlight-face highlight))

;;; 3rd-party

(package-faces 'alert
               '(alert-urgent   urgency-urgent)
               '(alert-high     urgency-high)
               '(alert-moderate urgency-moderate)
               '(alert-normal   urgency-normal)
               '(alert-low      urgency-low)
               '(alert-trivial  urgency-trivial))

;;; Ensime is a little crazier, as it has a weird alist for semantic faces.
(eval-after-load 'ensime
  (lambda ()
    (defface ensime-sem-high-var '((default :inherit scala-font-lock:var-face))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-val '((default :inherit font-lock-constant-face))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-var-field '((default :inherit ensime-sem-high-var))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-val-field '((default :inherit ensime-sem-high-val))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-function-call
      '((default :inherit font-lock-function-name-face))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-operator '((default :inherit font-lock-keyword-face))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-param ()
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-class '((default :inherit font-lock-type-face))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-trait '((default :inherit font-lock-type-face))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-object '((default :inherit font-lock-module-face))
      ""
      :group 'extended-faces)
    (defface ensime-sem-high-package '((default :inherit font-lock-module-face))
      ""
      :group 'extended-faces)

    (setq ensime-sem-high-faces
          '((var          . ensime-sem-high-var)
            (val          . ensime-sem-high-val)
            (varField     . ensime-sem-high-var-field)
            (valField     . ensime-sem-high-val-field)
            (functionCall . ensime-sem-high-function-call)
            (operator     . ensime-sem-high-operator)
            (param        . ensime-sem-high-param)
            (class        . ensime-sem-high-class)
            (trait        . ensime-sem-high-trait)
            (object       . ensime-sem-high-object)
            (package      . ensime-sem-high-package)))))

(package-faces 'helm
               '(helm-bookmark-directory    helm-ff-directory)
               '(helm-bookmark-file         helm-ff-file)
               '(helm-etags+-highlight-face highlight)
               '(helm-ff-directory          fs-directory)
               '(helm-ff-executable         fs-executable)
               '(helm-ff-file               fs-file)
               '(helm-ff-invalid-symlink    fs-broken-symlink)
               '(helm-ff-symlink            fs-symlink)
               '(helm-match                 match)
               '(helm-gentoo-match          helm-match)
               '(helm-grep-match            helm-match)
               '(helm-selection             region)
               '(helm-selection-line        secondary-selection)
               '(helm-source-header         helm-header)
               '(helm-w3m-bookmarks         helm-bookmark-w3m))

(package-faces 'idris-mode
               '(idris-active-term-face       isearch)
               '(idris-colon-face             font-lock-builtin-face)
               '(idris-definition-face        (sem-hi-binding sem-hi-scope-global font-lock-function-name-face))
               '(idris-equals-face            font-lock-builtin-face)
               '(idris-identifier-face        font-lock-identifier-face)
               '(idris-ipkg-keyword-face      font-lock-keyword-face)
               '(idris-ipkg-package-face      font-lock-module-face)
               '(idris-keyword-face           font-lock-keyword-face)
               ;; '(idris-loaded-region-face     font-lock-keyword)
               '(idris-log-level-1-face       urgency-urgent)
               '(idris-log-level-2-face       urgency-high)
               '(idris-log-level-3-face       urgency-moderate)
               '(idris-log-level-4-face       urgency-normal)
               '(idris-log-level-5-face       urgency-low)
               '(idris-log-level-higher-face  urgency-trivial)
               '(idris-metavariable-face      font-lock-variable-name-face)
               '(idris-module-face            font-lock-module-face)
               '(idris-operator-face          font-lock-operator-face)
               '(idris-parameter-face         font-lock-variable-name-face)
               ;; '(idris-semantic-bound-face    font-lock-operator-face)
               ;; '(idris-semantic-data-face     font-lock-operator-face)
               '(idris-semantic-function-face font-lock-function-name-face)
               ;; '(idris-semantic-implicit-face font-lock-operator-face)
               '(idris-semantic-type-face     font-lock-type-face)
               '(idris-unsafe-face            font-lock-warning-face)
               '(idris-warning-face           warning))

(package-faces 'lua2-mode
               '(lua2-error                     error)
               '(lua2-bind-variable             (sem-hi-binding sem-hi-scope-local font-lock-variable-name-face))
               '(lua2-reference-variable        (sem-hi-reference sem-hi-scope-local font-lock-variable-name-face))
               '(lua2-assign-variable           (sem-hi-mutable sem-hi-scope-local font-lock-variable-name-face))
               ;; NB: This one should probably use name hashing
               '(lua2-reference-global-variable (sem-hi-reference sem-hi-scope-global font-lock-variable-name-face))
               '(lua2-assign-global-variable    (sem-hi-mutable sem-hi-scope-global font-lock-variable-name-face)))

(package-faces 'markdown
               '(markdown-bold-face             bold)
               '(markdown-comment-face          font-lock-comment)
               '(markdown-header-delimiter-face shadow)
               '(markdown-header-face           text-heading)
               '(markdown-header-face-1         (level-1 markdown-header-face))
               '(markdown-header-face-2         (level-2 markdown-header-face))
               '(markdown-header-face-3         (level-3 markdown-header-face))
               '(markdown-header-face-4         (level-4 markdown-header-face))
               '(markdown-header-face-5         (level-5 markdown-header-face))
               '(markdown-header-face-6         (level-6 markdown-header-face))
               '(markdown-header-rule-face      shadow)
               '(markdown-italic-face           italic)
               '(markdown-link-face             shadow)
               '(markdown-link-title-face       link)
               '(markdown-url-face              link))

(package-faces 'parenface
               '(parenface-bracket-face shadow)
               '(parenface-curly-face   shadow)
               '(parenface-paren-face   shadow))

(package-faces 'paren-face
               '(parenthesis shadow))

(package-faces 'rainbow-delimiters
               '(rainbow-delimiters-depth-1-face level-1)
               '(rainbow-delimiters-depth-2-face level-2)
               '(rainbow-delimiters-depth-3-face level-3)
               '(rainbow-delimiters-depth-4-face level-4)
               '(rainbow-delimiters-depth-5-face level-5)
               '(rainbow-delimiters-depth-6-face level-6)
               '(rainbow-delimiters-depth-7-face level-7)
               '(rainbow-delimiters-depth-8-face level-8)
               '(rainbow-delimiters-depth-9-face level-9))

(package-faces 'rst-mode
               '(rst-level-1 (level-1 text-heading))
               '(rst-level-2 (level-2 text-heading))
               '(rst-level-3 (level-3 text-heading))
               '(rst-level-4 (level-4 text-heading))
               '(rst-level-5 (level-5 text-heading))
               '(rst-level-6 (level-6 text-heading)))

(package-faces 'scala-mode2
               '(scala-font-lock:abstract-face    font-lock-keyword-face)
               '(scala-font-lock:final-face       font-lock-keyword-face)
               '(scala-font-lock:implicit-face    font-lock-keyword-face)
               '(scala-font-lock:lazy-face        font-lock-keyword-face)
               '(scala-font-lock:override-face    font-lock-keyword-face)
               '(scala-font-lock:private-face     font-lock-keyword-face)
               '(scala-font-lock:protected-face   font-lock-keyword-face)
               '(scala-font-lock:sealed-face      font-lock-keyword-face)
               '(scala-font-lock:var-face         font-lock-variable-face)
               '(scala-font-lock:var-keyword-face font-lock-keyword-face))

(package-faces 'slime
               '(slime-error-face   error)
               '(slime-note-face    success)
               '(slime-warning-face warning))

(provide 'extended-faces)
