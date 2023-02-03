;;; interim-faces.el --- Inject extended faces -*- lexical-binding: t; -*-

;;; Commentary:

;; This module integrates `extended-faces' with many different packages, to
;; provide more consistent theming across all of Emacs.
;;
;; Packages that are referenced here do not directly depend on extended-faces,
;; but as they begin to, they should be removed from here.

;;; Code:

(require 'extended-faces)

(defun package-faces (package &rest faces)
  "Associates FACES with inheritance and the PACKAGE that they’re defined in.
This is largely so we can defer the definitions until the package is loaded.
Each element of FACES is a list containing the name of the face to modify
followed by a list of the faces that it should inherit from."
  (eval-after-load package
    `(mapcar (lambda (face) (apply #'set-face-inheritance face)) ',faces)))

;;; built-in
(package-faces 'apropos
               '(apropos-keybinding         ())
               '(apropos-misc-button        ())
               '(apropos-property           ())
               '(apropos-symbol             ())
               '(apropos-user-option-button ()))

(package-faces 'calendar
               '(calendar-month-header   text-heading)
               '(calendar-today          secondary-selection)
               '(calendar-weekday-header text-heading)
               '(calendar-weekend-header text-heading)
               '(holiday                 highlight))

(package-faces 'css-mode
               '(css-proprietary-property css-property))

(package-faces 'comint
               '(comint-highlight-input  input)
               '(comint-highlight-prompt prompt))

(package-faces 'custom
               '(custom-button          button)
               '(custom-button-pressed-unraised
                 (custom-button-unraised custom-button-pressed))
               '(custom-button-mouse    (custom-button button-mouseover))
               '(custom-button-pressed  (custom-button button-pressed))
               '(custom-button-unraised custom-button)
               '(custom-comment         font-lock-comment-face)
               '(custom-invalid         warning)
               '(custom-link            link)
               '(custom-variable-button custom-button))

(package-faces 'compilation
               '(compilation-info    success)
               '(compilation-warning warning))

(package-faces 'diary-lib
               '(diary-button        button))

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

(package-faces 'diff-mode
               '(diff-added             ())
               '(diff-changed           ())
               '(diff-context           ())
               '(diff-error             error)
               '(diff-file-header       (diff-header level-2))
               '(diff-function          font-lock-function-name-face)
               '(diff-header            text-heading)
               '(diff-hunk-header       (diff-header level-3))
               '(diff-index             diff-header)
               '(diff-indicator-added   diff-added)
               '(diff-indicator-changed diff-changed)
               '(diff-indicator-removed diff-removed)
               '(diff-nonexistent       diff-removed)
               '(diff-refine-added      diff-added)
               '(diff-refine-changed    diff-changed)
               '(diff-refine-removed    diff-removed)
               '(diff-removed           ()))

(package-faces 'dired
               '(dired-directory  fs-directory)
               '(dired-perm-write ()) ; NB: This is just to delete the default
               '(dired-symlink    fs-symlink))

(package-faces 'ediff
               '(ediff-current-diff-A        (diff-removed highlight))
               '(ediff-current-diff-Ancestor (diff-changed highlight))
               '(ediff-current-diff-B        (diff-added highlight))
               '(ediff-current-diff-C      (diff-changed-unspecified highlight))
               '(ediff-even-diff-A           (diff-removed secondary-selection))
               '(ediff-even-diff-Ancestor    (diff-changed secondary-selection))
               '(ediff-even-diff-B           (diff-added secondary-selection))
               '(ediff-even-diff-C
                 (diff-changed-unspecified secondary-selection))
               '(ediff-fine-diff-A           diff-refine-removed)
               '(ediff-fine-diff-Ancestor    diff-refine-changed)
               '(ediff-fine-diff-B           diff-refine-added)
               '(ediff-fine-diff-C           diff-changed-unspecified)
               '(ediff-odd-diff-A            (diff-removed shadow))
               '(ediff-odd-diff-Ancestor     (diff-changed shadow))
               '(ediff-odd-diff-B            (diff-added shadow))
               '(ediff-odd-diff-C            (diff-changed-unspecified shadow)))

(eval-after-load 'ediff
  '(default-mode-face 'fixed-pitch '(ediff-mode)))

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
               '(eshell-ls-symlink    fs-symlink)
               ;; '(eshell-ls-unreadable (,@fg-base00))
               '(eshell-prompt        prompt))

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
               '(info-xref-visited (link-visited info-xref)))

(package-faces 'message
               '(message-header-to         message-header-other)
               '(message-header-cc         message-header-other)
               '(message-header-subject    message-header-other)
               '(message-header-newsgroups message-header-other)
               '(message-header-xheader    message-header-other))

(package-faces 'minimap
               '(minimap-font-face font-lock) ; also set :height 30
               '(minimap-current-line-face highlight)
               '(minimap-active-region-background region)
               '(minimap-semantic-function-face font-lock-function-name-face)
               '(minimap-semantic-variable-face font-lock-variable-name-face)
               '(minimap-semantic-type-face font-lock-type-face))

(package-faces 'nxml-mode
               '(nxml-delimiter delimiter)
               '(nxml-text      text))

(package-faces 'nxml-outln
               '(nxml-heading text-heading))

(eval-after-load 'org
  '(progn
     (defeface org-agenda '((default (:inherit org-default)))
       ""
       :group 'org-faces)
     (defeface org-agenda-calendar '((default (:inherit org-agenda)))
       ""
       :group 'org-faces)))

(package-faces 'org
               '(org-agenda-calendar-event   org-agenda-calendar)
               '(org-agenda-calendar-sexp    org-agenda-calendar)
               '(org-agenda-current-time     (org-time-grid org-agenda))
               '(org-agenda-diary            org-agenda)
               '(org-agenda-dimmed-todo-face (shadow org-agenda))
               '(org-agenda-done             (org-done org-agenda))
               '(org-agenda-restriction-lock org-agenda)
               '(org-block                   (fixed-pitch org-default))
               '(org-block-background        (org-default
                                              fixed-pitch
                                              secondary-selection))
               '(org-checkbox                org-default)
               '(org-code                    (font-lock org-default))
               '(org-column-title            (text-heading org-default))
               '(org-default                 text)
               '(org-document-title          text-title)
               '(org-headline-done           org-done)
               '(org-level-1                 (outline-1 org-default))
               '(org-level-2                 (outline-2 org-default))
               '(org-level-3                 (outline-3 org-default))
               '(org-level-4                 (outline-4 org-default))
               '(org-level-5                 (outline-5 org-default))
               '(org-level-6                 (outline-6 org-default))
               '(org-level-7                 (outline-7 org-default))
               '(org-level-8                 (outline-8 org-default))
               '(org-link                    (link org-default))
               '(org-list-dt                 (text-definition-term org-default))
               '(org-scheduled               org-default)
               '(org-scheduled-previously    (urgency-urgent org-scheduled))
               '(org-scheduled-today         (urgency-high org-scheduled))
               '(org-table                   (fixed-pitch org-default))
               '(org-time-grid               org-default)
               '(org-upcoming-deadline       (urgency-moderate org-default))
               '(org-verbatim                (text-verbatim org-default))
               '(org-warning                 (warning org-default)))

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

(package-faces 'sh-script
               '(sh-heredoc     font-lock-string-face)
               '(sh-quoted-exec font-lock-string-face))

(package-faces 'smerge-mode
               '(smerge-base            diff-changed)
               '(smerge-lower           diff-added)
               ;; '(smerge-markers         )
               '(smerge-refined-added   diff-refine-added)
               '(smerge-refined-changed diff-refine-changed)
               '(smerge-refined-removed diff-refine-removed)
               '(smerge-upper           diff-removed))

(package-faces 'speedbar
               '(speedbar-button-face    button)
               '(speedbar-directory-face fs-directory)
               '(speedbar-file-face      fs-file)
               '(speedbar-highlight-face highlight))

(package-faces 'term
               '(term               fixed-pitch)
               '(term-bold          ansi-color-bold)
               '(term-color-black   ansi-color-black)
               '(term-color-red     ansi-color-red)
               '(term-color-green   ansi-color-green)
               '(term-color-yellow  ansi-color-yellow)
               '(term-color-blue    ansi-color-blue)
               '(term-color-magenta ansi-color-magenta)
               '(term-color-cyan    ansi-color-cyan)
               '(term-color-white   ansi-color-white)
               '(term-underline     ansi-color-underline))

(package-faces 'widget
               '(widget-button         button)
               '(widget-mouse-face     (widget-button button-mouseover))
               '(widget-button-pressed (widget-button button-pressed)))

;;; 3rd-party

(package-faces 'alert
               '(alert-urgent   urgency-urgent)
               '(alert-high     urgency-high)
               '(alert-moderate urgency-moderate)
               '(alert-normal   urgency-normal)
               '(alert-low      urgency-low)
               '(alert-trivial  urgency-trivial))

(package-faces 'darcsum
               '(darcsum-header-face      diff-header)
               '(darcsum-marked-face      diff-refine-changed)
               '(darcsum-need-action-face warning)
               '(darcsum-need-action-marked-face
                 (darcsum-marked-face darcsum-need-action-face))
               '(darcsum-filename-face    fs-file)
               '(darcsum-change-line-face diff-changed))

(eval-after-load 'emacs-wiki-colors
  '(defeface emacs-wiki-header '((default (:inherit text-heading)))
     ""
     :group 'emacs-wiki-highlight)) ;; FIXME: check if this group is right

(package-faces 'emacs-wiki-colors
               '(emacs-wiki-bad-link-face error)
               '(emacs-wiki-header-1      (level-1 emacs-wiki-header))
               '(emacs-wiki-header-2      (level-2 emacs-wiki-header))
               '(emacs-wiki-header-3      (level-3 emacs-wiki-header))
               '(emacs-wiki-header-4      (level-4 emacs-wiki-header))
               '(emacs-wiki-header-5      (level-5 emacs-wiki-header))
               '(emacs-wiki-link-face     link)
               '(emacs-wiki-verbatim-face text-verbatim))

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

(package-faces 'ensime
               '(ensime-breakpoint-face         breakpoint-enabled)
               '(ensime-implicit-highlight      font-lock-warning-face)
               '(ensime-marker-face             hl-line)
               '(ensime-pending-breakpoint-face breakpoint-disabled))

(package-faces 'flycheck
               '(flycheck-error   error)
               '(flycheck-info    message)
               '(flycheck-warning warning))

(package-faces 'flymake
               '(flymake-errline  error)
               '(flymake-warnline warning))

(package-faces 'flyspell
               '(flyspell-duplicate warning)
               '(flyspell-incorrect error))

;; (package-faces 'guide-key
;;                '(guide-key/prefix-command-face    )
;;                '(guide-key/highlight-command-face )
;;                '(guide-key/key-face               ))

(package-faces 'haskell-mode
               '(haskell-error-face    font-lock-warning-face)
               '(haskell-operator-face font-lock-operator-face)
               '(haskell-warning-face  font-lock-warning-face))
;; FIXME doesn’t derive from prog-mode for some reason
(default-mode-face 'font-lock '(haskell-cabal-mode))

(package-faces 'helm
               '(helm-bookmark-directory    helm-ff-directory)
               '(helm-bookmark-file         helm-ff-file)
               '(helm-etags+-highlight-face highlight)
               '(helm-ff-directory          fs-directory)
               '(helm-ff-executable         fs-executable)
               '(helm-ff-file               fs-file)
               '(helm-ff-invalid-symlink    fs-broken-symlink)
               '(helm-ff-symlink            fs-symlink)
               ;; '(helm-header) ; Already correct
               '(helm-match                 match)
               '(helm-gentoo-match          helm-match)
               '(helm-grep-match            helm-match)
               '(helm-selection             region)
               '(helm-selection-line        secondary-selection)
               '(helm-source-header         helm-header)
               '(helm-w3m-bookmarks         helm-bookmark-w3m))

(default-mode-face 'pseudo-column '(helm-major-mode))

(package-faces 'highlight-doxygen
               '(highlight-doxygen-code-block font-lock)
               '(highlight-doxygen-comment    font-lock-doc-face))

(package-faces 'hydra
               ;; TODO: These faces shouldn’t be named by color
               '(hydra-face-teal     green)   ; Approximately cmyk(f007)
               '(hydra-face-blue     cyan)    ; Approximately cmyk(ff00)
               '(hydra-face-pink     blue)    ; Approximately cmyk(0302)
               '(hydra-face-amaranth magenta) ; Approximately cmyk(0c91)
               '(hydra-face-red      red))    ; Approximately cmyk(0ff0)

(package-faces 'idris-mode
               '(idris-active-term-face       isearch)
               '(idris-colon-face             font-lock-builtin-face)
               '(idris-definition-face
                 (sem-hi-binding
                  sem-hi-scope-global
                  font-lock-function-name-face))
               '(idris-equals-face            font-lock-builtin-face)
               '(idris-identifier-face        font-lock-identifier-face)
               '(idris-ipkg-keyword-face      font-lock-keyword-face)
               '(idris-ipkg-package-face      font-lock-module-face)
               '(idris-keyword-face           font-lock-keyword-face)
               '(idris-loaded-region-face     highlight)
               '(idris-log-level-1-face   (urgency-urgent idris-log-level-face))
               '(idris-log-level-2-face     (urgency-high idris-log-level-face))
               '(idris-log-level-3-face (urgency-moderate idris-log-level-face))
               '(idris-log-level-4-face   (urgency-normal idris-log-level-face))
               '(idris-log-level-5-face      (urgency-low idris-log-level-face))
               '(idris-log-level-higher-face
                 (urgency-trivial idris-log-level-face))
               '(idris-metavariable-face      font-lock-variable-name-face)
               '(idris-module-face            font-lock-module-face)
               '(idris-operator-face          font-lock-operator-face)
               '(idris-parameter-face         font-lock-variable-name-face)
               '(idris-repl-input-face        input)
               '(idris-repl-output-face       output)
               '(idris-repl-prompt-face       prompt)
               '(idris-repl-result-face       result)
               '(idris-semantic-bound-face    sem-hi-bound)
               '(idris-semantic-data-face     font-lock-literal-face)
               '(idris-semantic-function-face font-lock-function-name-face)
               '(idris-semantic-implicit-face font-lock-variable-name-face)
               '(idris-semantic-type-face     font-lock-type-face)
               '(idris-unsafe-face            font-lock-warning-face)
               '(idris-warning-face           warning))

(package-faces 'js2-mode
               '(js2-external-variable-face        font-lock-warning-face)
               '(js2-jsdoc-html-tag-delimiter-face delimiter)
               '(js2-jsdoc-html-tag-name-face      font-lock-function-name-face)
               '(js2-jsdoc-tag-face                text-definition-term)
               '(js2-jsdoc-type-face               font-lock-type-face)
               '(js2-jsdoc-value-face              text-definition-explanation))

(package-faces 'lua2-mode
               '(lua2-error error)
               '(lua2-bind-variable
                 (sem-hi-binding
                  sem-hi-scope-local
                  font-lock-variable-name-face))
               '(lua2-reference-variable
                 (sem-hi-reference
                  sem-hi-scope-local
                  font-lock-variable-name-face))
               '(lua2-assign-variable
                 (sem-hi-mutable
                  sem-hi-scope-local
                  font-lock-variable-name-face))
               ;; NB: This one should probably use name hashing
               '(lua2-reference-global-variable
                 (sem-hi-reference
                  sem-hi-scope-global
                  font-lock-variable-name-face))
               '(lua2-assign-global-variable
                 (sem-hi-mutable
                  sem-hi-scope-global
                  font-lock-variable-name-face)))

(package-faces 'magit
               '(magit-diff-added         (magit-diff-file-contents diff-added))
               '(magit-diff-added-highlight        (magit-diff-added highlight))
               '(magit-diff-base        (magit-diff-file-contents diff-changed))
               '(magit-diff-base-highlight          (magit-diff-base highlight))
               '(magit-diff-conflict-heading        smerge-markers)
               '(magit-diff-context     (magit-diff-file-contents diff-context))
               '(magit-diff-context-highlight    (magit-diff-context highlight))
               '(magit-diff-diffstat-added          diff-indicator-added)
               '(magit-diff-diffstat-removed        diff-indicator-removed)
               ;; NB: Ensure ‘magit-diff-file-contents’ is ‘fixed-pitch’ because
               ;;     having columns align is generally useful in a diff,
               ;;     regardless of the type of content.
               '(magit-diff-file-contents           fixed-pitch)
               '(magit-diff-file-heading            diff-file-header)
               '(magit-diff-hunk-heading            diff-hunk-header)
               '(magit-diff-hunk-heading-highlight  magit-section-highlight)
               '(magit-diff-hunk-heading-selection  selection)
               '(magit-diff-removed     (magit-diff-file-contents diff-removed))
               '(magit-diff-removed-highlight
                 (magit-diff-removed magit-section-highlight))
               '(magit-diff-revision-summary        text-title)
               '(magit-diff-revision-highlight      magit-section-highlight)
               ;; NB: This often affects alignment of the ASCII graph
               '(magit-hash                         pseudo-column)
               '(magit-key-mode-button-face         button)
               '(magit-key-mode-header-face         text-heading)
               '(magit-log-author                   ())
               ;; NB: This is drawn as ASCII art
               '(magit-log-graph                    fixed-pitch)
               '(magit-log-reflog-label-checkout   magit-log-reflog-label-other)
               '(magit-log-reflog-label-cherry-pick
                 magit-log-reflog-label-other)
               '(magit-log-reflog-label-commit     magit-log-reflog-label-other)
               '(magit-log-reflog-label-reset      magit-log-reflog-label-other)
               '(magit-log-reflog-label-rebase     magit-log-reflog-label-other)
               '(magit-log-reflog-label-remote     magit-log-reflog-label-other)
               '(magit-process-ng                   (error magit-section-title))
               '(magit-process-ok                (success magit-section-title))
               '(magit-section-heading              level-1)
               '(magit-section-highlight            highlight)
               '(magit-signature-bad                (red magit-hash))
               '(magit-signature-error              (yellow magit-hash))
               '(magit-signature-expired            (cyan magit-hash))
               '(magit-signature-expired-key        (cyan magit-hash))
               '(magit-signature-good               (blue magit-hash))
               '(magit-signature-revoked            (magenta magit-hash))
               '(magit-signature-untrusted          (green magit-hash)))

(eval-after-load 'magit
  '(progn
     (default-mode-face 'fixed-pitch
       '(magit-log-mode ; uses columns, but not enough
         magit-popup-mode
         magit-refs-mode))))

(package-faces 'markdown-mode
               '(markdown-bold-face             bold)
               '(markdown-comment-face          font-lock-comment-face)
               '(markdown-header-delimiter-face delimiter)
               '(markdown-header-face           text-heading)
               '(markdown-header-face-1         (level-1 markdown-header-face))
               '(markdown-header-face-2         (level-2 markdown-header-face))
               '(markdown-header-face-3         (level-3 markdown-header-face))
               '(markdown-header-face-4         (level-4 markdown-header-face))
               '(markdown-header-face-5         (level-5 markdown-header-face))
               '(markdown-header-face-6         (level-6 markdown-header-face))
               '(markdown-header-rule-face      shadow)
               '(markdown-inline-code-face      font-lock)
               '(markdown-italic-face           italic)
               '(markdown-language-keyword-face font-lock-keyword-face)
               '(markdown-link-face             shadow)
               '(markdown-link-title-face       link)
               '(markdown-metadata-key-face     text-definition-term)
               '(markdown-metadata-value-face   text-definition-explanation)
               '(markdown-pre-face              text-verbatim)
               '(markdown-url-face              link))

(package-faces 'paradox
               '(paradox-comment-face   font-lock-comment-face)
               '(paradox-highlight-face highlight))

(package-faces 'parenface
               '(parenface-bracket-face shadow)
               '(parenface-curly-face   shadow)
               '(parenface-paren-face   shadow))

(package-faces 'paren-face
               '(parenthesis shadow))

(package-faces 'popup
               '(popup-face ())
               '(popup-isearch-match              (match popup-face))
               '(popup-scroll-bar-background-face (scroll-bar popup-face))
               '(popup-scroll-bar-foreground-face (scroll-bar popup-face))
               '(popup-summary-face               popup-face)
               '(popup-tip-face                   popup-face))

(package-faces 'psvn
               '(svn-status-directory-face fs-directory)
               '(svn-status-filename-face  fs-file)
               '(svn-status-symlink-face   fs-symlink))

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

(eval-after-load 'scala-mode2
  '(defeface scala-font-lock:keyword-face
     '((default (:inherit font-lock-keyword-face)))
     "Font Lock mode face used for keywords."
     :group 'scala))

(package-faces 'scala-mode2
               '(scala-font-lock:abstract-face    scala-font-lock:keyword-face)
               '(scala-font-lock:final-face       scala-font-lock:keyword-face)
               '(scala-font-lock:implicit-face    scala-font-lock:keyword-face)
               '(scala-font-lock:lazy-face        scala-font-lock:keyword-face)
               '(scala-font-lock:override-face    scala-font-lock:keyword-face)
               '(scala-font-lock:private-face     scala-font-lock:keyword-face)
               '(scala-font-lock:protected-face   scala-font-lock:keyword-face)
               '(scala-font-lock:sealed-face      scala-font-lock:keyword-face)
               '(scala-font-lock:var-face         font-lock-variable-name-face)
               '(scala-font-lock:var-keyword-face scala-font-lock:keyword-face))

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

(package-faces 'slime
               '(sldb-topline-face                 (slime-topline sldb-default))
               '(sldb-condition-face                  sldb-default)
               '(sldb-section-face                    sldb-default)
               '(sldb-frame-label-face                sldb-default)
               '(sldb-restart-face                    sldb-default)
               '(sldb-restart-number-face             sldb-restart-face)
               '(sldb-restart-type-face               sldb-restart-face)
               '(sldb-frame-line-face                 sldb-default)
               '(sldb-restartable-frame-line-face     sldb-frame-line-face)
               '(sldb-non-restartable-frame-line-face sldb-frame-line-face)
               '(sldb-detailed-frame-line-face        sldb-frame-line-face)
               '(sldb-local-name-face                 sldb-default)
               '(sldb-local-value-face                sldb-default)
               '(sldb-catch-tag-face                  sldb-default)
               '(slime-apropos-label                  slime-apropos-default)
               '(slime-apropos-symbol                 slime-apropos-default)
               '(slime-error-face                     (error slime-default))
               '(slime-highlight-face                 (highlight slime-default))
               '(slime-inspector-topline-face
                 (slime-topline slime-inspector-default))
               '(slime-inspector-label-face           slime-inspector-default)
               '(slime-inspector-value-face           slime-inspector-default)
               '(slime-inspector-action-face          slime-inspector-default)
               '(slime-inspector-type-face            slime-inspector-default)
               '(slime-note-face                      (message slime-default))
               '(slime-warning-face                   (warning slime-default))
               '(slime-style-warning-face             slime-warning-face))

(package-faces 'slime-repl
               '(slime-repl-input-face          input)
               '(slime-repl-inputed-output-face slime-repl-output-face)
               '(slime-repl-output-face         output)
               '(slime-repl-prompt-face         prompt)
               '(slime-repl-result-face         result))

(package-faces 'transient
               ;; colors first
               '(transient-teal              green)   ; Approximately cmyk(f007)
               '(transient-blue              cyan)    ; Approximately cmyk(ff00)
               '(transient-purple            blue)    ; Approximately cmyk(0f07)
               '(transient-pink              magenta) ; Approximately cmyk(0302)
               '(transient-amaranth          red)     ; Approximately cmyk(0c91)
               '(transient-red               yellow)  ; Approximately cmyk(0ff0)
               ;; '(transient-active-infix      ())
               ;; '(transient-argument          ())
               ;; '(transient-disabled-suffix   ())
               ;; '(transient-enabled-suffix    ())
               ;; '(transient-heading           ())
               ;; '(transient-higher-level      ())
               ;; '(transient-inactive-argument) ; Already correct
               ;; '(transient-inactive-value) ; Already correct
               ;; '(transient-inapt-suffix      ())
               ;; See ‘transient-align-variable-pitch’
               '(transient-key             (font-lock-builtin-face fixed-pitch))
               ;; These two are documented to inherit from ‘transient-key’,
               ;; but they don’t actually.
               '(transient-mismatched-key    (warning transient-key))
               '(transient-nonstandard-key   (warning transient-key))
               '(transient-separator         mode-line)
               ;; '(transient-unreachable) ; Already correct
               '(transient-unreachable-key
                 (transient-unreachable transient-key))
               ;; '(transient-value             ())
               )

(eval-after-load 'whitespace
  '(defeface whitespace-default '((default (:inherit warning)))
     ""
     :group 'whitespace))

(package-faces 'whitespace
               '(whitespace-space            whitespace-default)
               '(whitespace-hspace           whitespace-default)
               '(whitespace-tab              whitespace-default)
               '(whitespace-newline          whitespace-default)
               '(whitespace-trailing         whitespace-default)
               '(whitespace-line             whitespace-default)
               '(whitespace-space-before-tab whitespace-default)
               '(whitespace-indentation      whitespace-default)
               '(whitespace-empty            whitespace-default)
               '(whitespace-space            whitespace-default)
               '(whitespace-space-after-tab  whitespace-default))

(package-faces 'write-good
               '(writegood-duplicates-face    warning)
               '(writegood-passive-voice-face warning)
               '(writegood-weasels-face       warning))

(provide 'interim-faces)
;;; interim-faces.el ends here
