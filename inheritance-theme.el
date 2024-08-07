;;; inheritance-theme.el --- A theme with only ‘:inherit’ relationships  -*- lexical-binding: t; -*-

;;; Commentary:

;; Notes on file contents

;; • most things are ordered alphabetically, but colors are ordered by CMYK
;; • sections are alphabetical by Custom group name with built-ins first and
;;   3rd-party second

;; Some standard comments:

;; “*” and “**”
;; section heading

;; “FIXME: aligning original”
;; We define the face, but only to make the original face fit in better with
;; various themes. E.g., if something has a face with a bunch of :foreground
;; variants, like "Cyan1" "Cyan" "Cyan" "Purple "LightGray", we define the face
;; instead to inherit the ‘cyan’ face which will adjust with each theme’s notion
;; of that face.

;; “FIXME: opinionated”
;; This setting does more than it should, probably an indicator of a missing
;; “interim” face.

;; “already correct”
;; The standard definition matches what we would define here, so we don’t define
;; it. The entry still exists to make it clear that we have checked the standard
;; face and not just missed it.

;;; Code:

(require 'cus-theme)

(defun inheritance-root-faces ()
  "Faces in the ‘inheritance’ theme that are roots of the graph.
I.e., they don’t explicitly inherit from another face. These are the faces you
are most likely to want to customize in a new theme."
  (let ((settings (get 'inheritance 'theme-settings))
        (faces))
    (dolist (s settings faces)          ; '(prop symbol theme value)
      (when (eq (car s) 'theme-face)
        (let ((symbol (nth 1 s))
              (value (nth 3 s)))
          (when (and (symbol-file symbol 'defface)
                     (null (plist-get (cadar value) :inherit)))
            (push symbol faces)))))))

(defun inheritance-create-theme-from-roots ()
  "Create a new theme from the “root” faces of the ‘inheritance’ theme."
  (let ((custom-theme--listed-faces (inheritance-root-faces)))
    (customize-create-theme)))

(deftheme inheritance
  "A DAG of faces. Created 2023-02-02.")

(apply #'custom-theme-set-faces
       'inheritance
       (mapcar
        (lambda (spec)
          (list (car spec)
                `((default ,(append (caddr spec) `(:inherit ,(cadr spec)))))))
        ;; Each element of this list should have the form
        ;;   (FACE-NAME (INHERITED-FACE…) [(OTHER-SPEC-COMPONENT…)])
        ;; OTHER-SPEC-COMPONENTs should be rare – the intent of this theme is
        ;; inheritance. But since this also (intentionally) tramples all of the
        ;; “default” faces, we sometimes need to restore some attributes.
        ;;
        ;; Never mention
        ;; • family
        ;; • foundry
        ;; • colors (:foregound, ::distant-foreground, background, :color)
        ;; • stipple
        ;; • font
        ;;
        ;; Ok to mention
        ;; • extend
        ;; • inverse-video
        ;;
        ;; Restricted
        ;; • width
        ;; • height (only ever relative)
        ;; • weight
        ;; • slant
        ;; • underline
        ;; • overline
        ;; • strike-through
        ;; • box
        '(;; * built-in

          ;; ** basic-faces
          ;; default – The default face, whose attributes are all specified.

          ;; bold      – These have the attributes indicated by their names
          ;; italic    –  (e.g., bold has `:weight bold`), with all other
          ;; underline –  attributes unspecified (and so given by default).

          ;; fixed-pitch       – These should have their ‘family’ explicitly set
          ;; fixed-pitch-serif – by the user.
          ;; variable-pitch

          (bold-italic            (bold italic))
          (default                ())
          (error                  (message))
          (escape-glyph           ())
          (glyphless-char         ())
          (header-line            (mode-line-inactive))
          (header-line-highlight  (highlight header-line))
          (highlight              ())
          (homoglyph              (warning))
          (link                   ())
          (link-visited           (link))
          (minibuffer-prompt      (prompt))
          (nobreak-hyphen         ())
          (nobreak-space          ())
          (region                 (highlight) (:extend t))
          (secondary-selection    (highlight) (:extend t))
          (shadow                 ())
          (success                (message))
          (tab-bar                ())
          (tab-line               ())
          (tool-bar               ())
          (trailing-whitespace    ())
          ;; same as standard, but not limited to ‘tty’
          (vertical-border        (mode-line-inactive))
          (warning                (message))

          ;; ** cursor
          (cursor ())
          ;; ** display-fill-column-indicator
          (fill-column-indicator (warning))
          ;; ** display-line-numbers
          (line-number            (pseudo-column fringe))
          ;; (line-number-current-line (line-number)) ; already correct
          (line-number-major-tick (line-number))
          (line-number-minor-tick (line-number))
          ;; ** frames
          ;; (border              ()) ; already correct
          (child-frame-border  (internal-border))
          (fringe              ())
          ;; (internal-border     ()) ; already correct
          (scroll-bar          ())
          ;; ** menu
          (menu ())
          ;; ** mode-line-faces
          (mode-line            ())
          ;; layered on both mode-line and mode-line-inactive
          (mode-line-buffer-id  ())
          (mode-line-emphasis   text-emphasis)
          (mode-line-inactive   mode-line)
          (mode-line-highlight  (highlight mode-line))
          ;; ** mouse
          ;; (mouse ()) ; already correct
          ;; ** window-divider
          ;; NB: You might expect this to inherit from ‘mode-line’, but it uses
          ;;     the _foreground_ color, so ‘default’ is generally a better
          ;;     option.
          (window-divider             ())
          (window-divider-first-pixel (window-divider))
          (window-divider-last-pixel  (window-divider))

          ;; ** ansi-color
          ;; TODO: These also need their background color set to the same as the
          ;;       foreground.
          (ansi-color-cyan           (cyan))
          (ansi-color-blue           (blue))
          (ansi-color-magenta        (magenta))
          (ansi-color-red            (red))
          (ansi-color-yellow         (yellow))
          (ansi-color-green          (green))
          (ansi-color-black          (black))
          (ansi-color-white          (white))
          (ansi-color-bright-cyan    (bold cyan))
          (ansi-color-bright-blue    (bold blue))
          (ansi-color-bright-magenta (bold magenta))
          (ansi-color-bright-red     (bold red))
          (ansi-color-bright-yellow  (bold yellow))
          (ansi-color-bright-green   (bold green))
          (ansi-color-bright-black   (bold black))
          (ansi-color-bright-white   (bold white))
          (ansi-color-bold           (bold))
          (ansi-color-italic         (italic))
          (ansi-color-underline      (underline))

          ;; ** apropos
          (apropos-keybinding         (help-key-binding))
          ;; (apropos-misc-button        ())
          ;; (apropos-property           ())
          ;; (apropos-symbol             ())
          (apropos-user-option-button (button))

          ;; ** calendar-faces
          (calendar-month-header   (bold)) ; FIXME: opinionated
          (calendar-today          (match))
          (calendar-weekday-header () (:underline t)) ; FIXME: opinionated
          (calendar-weekend-header () (:underline t)) ; FIXME: opinionated
          (diary                   (red)) ; FIXME aligning original
          (holiday                 (highlight))

          ;; ** css-mode
          (css-proprietary-property (css-property))

          ;; ** comint
          (comint-highlight-input  (input))
          (comint-highlight-prompt (prompt))

          ;; ** custom
          (custom-button                  (button))
          (custom-button-pressed-unraised (link-visited custom-button-unraised))
          (custom-button-mouse            (button-mouseover custom-button))
          (custom-button-pressed          (button-pressed custom-button))
          (custom-button-unraised         (link))
          (custom-comment                 (font-lock-comment-face))
          (custom-documentation           (font-lock-doc-face))
          (custom-invalid                 (warning))
          (custom-link                    (link))
          (custom-variable-button         (custom-button))

          ;; ** compilation
          (compilation-column-number  ())
          (compilation-info           (success))
          (compilation-line-number    (line-number))
          (compilation-mode-line-exit (compilation-info))
          (compilation-mode-line-fail (compilation-error))
          (compilation-mode-line-run  ())
          (compilation-warning        (warning))

          ;; ** diary-lib
          (diary-button button)

          ;; ** diff-mode
          (diff-added             (diff-file-contents))
          (diff-changed           (diff-file-contents))
          (diff-context           (diff-file-contents))
          (diff-error             error)
          (diff-file-header       (level-2 diff-header))
          (diff-function          font-lock-function-name-face)
          (diff-header            text-heading)
          (diff-hunk-header       (level-3 diff-header))
          (diff-index             diff-header)
          (diff-indicator-added   diff-added)
          (diff-indicator-changed diff-changed)
          (diff-indicator-removed diff-removed)
          (diff-nonexistent       diff-removed)
          (diff-refine-added      diff-added)
          (diff-refine-changed    diff-changed)
          (diff-refine-removed    diff-removed)
          (diff-removed           (diff-file-contents))

          ;; ** dired
          (dired-broken-symlink (fs-broken-symlink dired-symlink))
          (dired-directory      (fs-directory))
          (dired-perm-write     (warning))
          (dired-set-id         (warning))
          (dired-special        ())
          (dired-symlink        (fs-symlink))

          ;; ** ediff
          (ediff-current-diff-A        (diff-removed highlight))
          (ediff-current-diff-Ancestor (diff-changed highlight))
          (ediff-current-diff-B        (diff-added highlight))
          (ediff-current-diff-C        (diff-changed-unspecified highlight))
          (ediff-even-diff-A           (diff-removed even))
          (ediff-even-diff-Ancestor    (diff-changed even))
          (ediff-even-diff-B           (diff-added even))
          (ediff-even-diff-C           (diff-changed-unspecified even))
          (ediff-fine-diff-A           (diff-refine-removed))
          (ediff-fine-diff-Ancestor    (diff-refine-changed))
          (ediff-fine-diff-B           (diff-refine-added))
          (ediff-fine-diff-C           (diff-changed-unspecified))
          (ediff-odd-diff-A            (diff-removed odd))
          (ediff-odd-diff-Ancestor     (diff-changed odd))
          (ediff-odd-diff-B            (diff-added odd))
          (ediff-odd-diff-C            (diff-changed-unspecified odd))

          ;; ** eshell
          ;; (eshell-ls-archive (,@fg-magenta))
          ;; (eshell-ls-backup (,@fg-yellow))
          (eshell-ls-clutter    (dired-ignored))
          (eshell-ls-directory  (fs-directory))
          (eshell-ls-executable (fs-executable))
          (eshell-ls-missing    (fs-broken-symlink))
          ;; (eshell-ls-product (,@fg-yellow))
          ;; (eshell-ls-readonly (,@fg-base1))
          (eshell-ls-special    (dired-special))
          (eshell-ls-symlink    (fs-symlink))
          ;; (eshell-ls-unreadable (,@fg-base00))
          (eshell-prompt        (prompt))

          ;; ** font-lock
          (elisp-shorthand-font-lock-face    (shadow font-lock-identifier-face))
          ;; TODO: This doesn’t seem to be used necessarily for built-in
          ;;       functions, but has some other purpose. So I’m not sure it
          ;;       should inherit ‘font-lock-function-name-face’.
          (font-lock-builtin-face              (font-lock-function-name-face))
          (font-lock-comment-delimiter-face  (delimiter font-lock-comment-face))
          (font-lock-comment-face              (font-lock-doc-face))
          (font-lock-constant-face             (font-lock-value-face))
          (font-lock-doc-face                  ())
          ;; XEmacs only
          (font-lock-doc-string-face           (font-lock-doc-face))
          (font-lock-function-name-face        (font-lock-identifier-face))
          (font-lock-keyword-face              ())
          (font-lock-negation-char-face        (font-lock-operator-face))
          (font-lock-preprocessor-face         ())
          (font-lock-regexp-grouping-backslash ())
          (font-lock-regexp-grouping-construct ())
          (font-lock-string-face               (font-lock-literal-face))
          (font-lock-type-face                 (font-lock-value-face))
          (font-lock-variable-name-face        (font-lock-identifier-face))
          (font-lock-warning                   (warning))

          ;; ** gnus
          (gnus-cite-1               level-1)
          (gnus-cite-2               level-2)
          (gnus-cite-3               level-3)
          (gnus-cite-4               level-4)
          (gnus-cite-5               level-5)
          (gnus-cite-6               level-6)
          (gnus-cite-7               level-7)
          (gnus-cite-8               level-8)
          (gnus-cite-9               level-9)
          (gnus-cite-10              level-10)
          (gnus-cite-11              level-11)
          (gnus-emphasis-bold            (bold))
          (gnus-emphasis-italic          (italic))
          (gnus-emphasis-underline       (underline))
          (gnus-emphasis-underline-bold
           (gnus-emphasis-underline gnus-emphasis-bold))
          (gnus-emphasis-underline-italic
           (gnus-emphasis-underline gnus-emphasis-italic))
          (gnus-emphasis-bold-italic
           (gnus-emphasis-bold gnus-emphasis-italic bold-italic))
          (gnus-emphasis-underline-bold-italic
           (gnus-emphasis-underline gnus-emphasis-bold-italic))
          (gnus-emphasis-strikethru      (strike-through))
          (gnus-emphasis-highlight-words (highlight))
          (gnus-header-content       (text-definition-explanation))
          (gnus-header-from          (text-author gnus-header-content))
          (gnus-header-name          (text-definition-term))
          (gnus-header-newsgroups    (gnus-header-content))
          (gnus-header-subject       (text-title))
          (gnus-summary-high-ancient (urgency-high gnus-summary-normal-ancient))
          (gnus-summary-high-read    (urgency-high gnus-summary-normal-read))
          (gnus-summary-high-ticked  (urgency-high gnus-summary-normal-ticked))
          (gnus-summary-high-undownloaded
           (urgency-high gnus-summary-normal-undownloaded))
          (gnus-summary-high-unread  (urgency-high gnus-summary-normal-unread))
          (gnus-summary-low-ancient  (urgency-low gnus-summary-normal-ancient))
          (gnus-summary-low-read     (urgency-low gnus-summary-normal-read))
          (gnus-summary-low-ticked   (urgency-low gnus-summary-normal-ticked))
          (gnus-summary-low-undownloaded
           (urgency-low gnus-summary-normal-undownloaded))
          (gnus-summary-low-unread   (urgency-low gnus-summary-normal-unread))

          ;; ** help
          (help-argument-name    (font-lock-identifier-face))
          (help-key-binding      ())
          (help-for-help-header  (text-heading))
          (tutorial-warning-face (warning))

          ;; ** icomplete
          (icomplete-first-match    (match))
          (icomplete-section        (completions-group-title))
          ;; (icomplete-selected-match (highlight)) ; already correct

          ;; ** info
          (info-title-1      (level-1 text-heading))
          (info-title-2      (level-2 text-heading))
          (info-title-3      (level-3 text-heading))
          (info-title-4      (level-4 text-heading))
          (info-xref         (link))
          (info-xref-visited (link-visited info-xref))

          ;; ** isearch
          (isearch         (match))
          (isearch-fail    (error))
          (isearch-group-1 (odd isearch))
          (isearch-group-2 (even isearch))
          (lazy-highlight  (highlight))

          ;; ** linum
          ;; NB: This inheritance can’t go the other way, as this face is
          ;;     lazily-loaded.
          (linum (line-number)
                 ;; NB: The display margins inherit the face of the adjacent
                 ;;     text in the text area (see
                 ;;     https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Margins.html),
                 ;;     so this does more than is desirable in order to prevent
                 ;;     leakage (especially of attributes that can affect glyph
                 ;;     width). Unfortunately, this doesn’t mix well with the
                 ;;     inherited faces. We really just want to set the ones
                 ;;     that aren’t set via inheritance.
                 ( :box nil
                   :extend nil
                   :height (lambda (_) (face-attribute 'default :height))
                   :inverse-video nil
                   :overline nil
                   :slant normal
                   :stipple nil
                   :strike-through nil
                   :underline nil
                   :weight normal
                   :width normal))

          ;; ** message
          (message-header-to         message-header-other)
          (message-header-cc         message-header-other)
          (message-header-subject    message-header-other)
          (message-header-newsgroups message-header-other)
          (message-header-xheader    message-header-other)

          ;; ** minibuffer
          ;; (completions-annotations      ())
          (completions-common-part      (blue)) ; FIXME: aligning original
          ;; (completions-first-difference ())
          (completions-group-separator  (delimiter)
                                        (:extend t :strike-through t))
          (completions-group-title      (level-1 text-heading))
          ;; (file-name-shadow             (shadow)) ; already correct
          (minibuffer-depth-indicator   (highlight minibuffer-prompt))

          ;; ** minimap
          (minimap-font-face                (font-lock) (:height 0.25))
          (minimap-current-line-face        (highlight))
          (minimap-active-region-background (region))
          (minimap-semantic-function-face   (font-lock-function-name-face))
          (minimap-semantic-variable-face   (font-lock-variable-name-face))
          (minimap-semantic-type-face       (font-lock-type-face))

          ;; ** next-error
          (next-error (highlight) (:extend t))

          ;; ** nxml-mode
          (nxml-delimiter delimiter)
          (nxml-text      text)

          ;; ** nxml-outln
          (nxml-heading text-heading)

          ;; ** org
          (org-agenda-calendar-event   org-agenda-calendar)
          (org-agenda-calendar-sexp    org-agenda-calendar)
          (org-agenda-current-time     (org-time-grid org-agenda))
          (org-agenda-diary            org-agenda)
          (org-agenda-dimmed-todo-face (shadow org-agenda))
          (org-agenda-done             (org-done org-agenda))
          (org-agenda-restriction-lock org-agenda)
          (org-block                   (fixed-pitch org-default))
          (org-block-background   (fixed-pitch secondary-selection org-default))
          (org-checkbox                org-default)
          (org-code                    (font-lock org-verbatim))
          (org-column-title            (text-heading org-default))
          (org-default                 (text))
          (org-document-info           (text-definition-explanation))
          (org-document-info-keyword   (text-definition-term))
          (org-document-title          (text-title))
          (org-headline-done           org-done)
          (org-level-1                 (outline-1 org-default))
          (org-level-2                 (outline-2 org-default))
          (org-level-3                 (outline-3 org-default))
          (org-level-4                 (outline-4 org-default))
          (org-level-5                 (outline-5 org-default))
          (org-level-6                 (outline-6 org-default))
          (org-level-7                 (outline-7 org-default))
          (org-level-8                 (outline-8 org-default))
          (org-link                    (link org-default))
          (org-list-dt                 (text-definition-term org-default))
          (org-scheduled               org-default)
          (org-scheduled-previously    (urgency-urgent org-scheduled))
          (org-scheduled-today         (urgency-high org-scheduled))
          (org-table                   (table org-default))
          (org-time-grid               org-default)
          (org-upcoming-deadline       (urgency-moderate org-default))
          (org-verbatim                (text-verbatim org-default))
          (org-warning                 (warning org-default))

          ;; ** outline
          (outline-1 level-1)
          (outline-2 level-2)
          (outline-3 level-3)
          (outline-4 level-4)
          (outline-5 level-5)
          (outline-6 level-6)
          (outline-7 level-7)
          (outline-8 level-8)

          ;; ** paren-showing-faces
          (show-paren-match    (match))
          ;; (show-paren-match-expression (show-paren-match)) ; already correct
          (show-paren-mismatch (error))

          ;; ** rcirc
          (rcirc-bright-nick               (highlight rcirc-other-nick))
          (rcirc-dim-nick                  (shadow rcirc-other-nick))
          (rcirc-keyword                   (highlight))
          (rcirc-my-nick                   (blue)) ; FIXME: aligning original
          (rcirc-nick-in-message           (match rcirc-my-nick))
          (rcirc-nick-in-message-full-line (highlight))
          (rcirc-other-nick                (yellow)) ; FIXME: aligning original
          (rcirc-prompt                    (prompt))
          (rcirc-server                    (alert-normal))
          (rcirc-server-prefix             (rcirc-server))
          ;; (rcirc-timestamp                 ()) ; already correct
          (rcirc-track-keyword             (alert-normal))
          (rcirc-track-nick                (alert-moderate))
          (rcirc-url                       (link))

          ;; ** replace
          (match (highlight))

          ;; ** sh-script
          (sh-heredoc     font-lock-string-face)
          (sh-quoted-exec font-lock-string-face)

          ;; ** smerge-mode
          (smerge-base            (diff-changed))
          (smerge-lower           (diff-added))
          (smerge-markers         (diff-hunk-header))
          (smerge-refined-added   (diff-refine-added))
          (smerge-refined-changed (diff-refine-changed))
          (smerge-refined-removed (diff-refine-removed))
          (smerge-upper           (diff-removed))

          ;; ** speedbar
          (speedbar-button-face    (button))
          (speedbar-directory-face (fs-directory))
          (speedbar-file-face      (fs-file))
          (speedbar-highlight-face (button-mouseover speedbar-button-face))
          (speedbar-selected-face  (highlight))
          (speedbar-separator-face (level-1 text-heading))
          (speedbar-tag-face       (blue)) ; FIXME: aligning original

          ;; ** term
          (term                      (fixed-pitch))
          ;; (term-color-cyan           (ansi-color-cyan)) ; already correct
          ;; (term-color-blue           (ansi-color-blue)) ; already correct
          ;; (term-color-magenta        (ansi-color-magenta)) ; already correct
          ;; (term-color-red            (ansi-color-red)) ; already correct
          ;; (term-color-yellow         (ansi-color-yellow)) ; already correct
          ;; (term-color-green          (ansi-color-green)) ; already correct
          ;; (term-color-black          (ansi-color-black)) ; already correct
          ;; (term-color-white          (ansi-color-white)) ; already correct
          ;; already correct
          ;; (term-color-bright-cyan    (ansi-color-bright-cyan))
          ;; already correct
          ;; (term-color-bright-blue    (ansi-color-bright-blue))
          ;; already correct
          ;; (term-color-bright-magenta (ansi-color-bright-magenta))
          ;; already correct
          ;; (term-color-bright-red     (ansi-color-bright-red))
          ;; already correct
          ;; (term-color-bright-yellow  (ansi-color-bright-yellow))
          ;; already correct
          ;; (term-color-bright-green   (ansi-color-bright-green))
          ;; already correct
          ;; (term-color-bright-black   (ansi-color-bright-black))
          ;; already correct
          ;; (term-color-bright-white   (ansi-color-bright-white))
          ;; (term-bold                 (ansi-color-bold)) ; already correct
          ;; already correct
          ;; (term-underline            (ansi-color-underline))

          ;; ** vc
          (log-edit-header         (level-2))
          (log-edit-summary        (text-title))
          (log-edit-unknown-header (log-edit-header))
          (vc-dir-directory        (fs-directory))
          (vc-dir-file             (fs-file))
          (vc-dir-header           (level-1))
          (vc-dir-header-value     (vc-dir-header))
          (vc-dir-status-warning   (warning))

          ;; ** widget
          (widget-button            (button))
          (widget-field             () (:extend t))
          (widget-mouse-face        (widget-button button-mouseover))
          (widget-button-pressed    (widget-button button-pressed))
          (widget-single-line-field (widget-field))

          ;; * third-party – nothing above here should inherit from anything
          ;;                below here, and in most cases, things shouldn’t
          ;;                inherit across packages below here.

          ;; ** ace-window
          (aw-background-face               (shadow) (:extend t))
          (aw-key-face                      (help-key-binding))
          (aw-leading-char-face             (help-key-binding))
          (aw-minibuffer-leading-char-face  (aw-leading-char-face))
          (aw-mode-line-face                (aw-leading-char-face mode-line))

          ;; ** alert
          (alert-urgent   urgency-urgent)
          (alert-high     urgency-high)
          (alert-moderate urgency-moderate)
          (alert-normal   urgency-normal)
          (alert-low      urgency-low)
          (alert-trivial  urgency-trivial)

          ;; ** auto-dim-other-buffers
          ;; TODO: This should be like ‘fringe’, but shouldn’t affect the
          ;;       foreground.
          (auto-dim-other-buffers-face      ())
          ;; TODO: This needs to have ‘:background’ set to match ‘:foreground’,
          ;;       similar to the ‘ansi-color’ faces.
          (auto-dim-other-buffers-hide-face (auto-dim-other-buffers-face))

          ;; ** bm
          (bm-face                   ())
          (bm-fringe-face            (bm-face fringe))
          (bm-fringe-persistent-face (bm-persistent-face fringe))
          (bm-persistent-face        (bm-face))

          ;; ** darcsum
          (darcsum-header-face      diff-header)
          (darcsum-marked-face      diff-refine-changed)
          (darcsum-need-action-face warning)
          (darcsum-need-action-marked-face
           (darcsum-marked-face darcsum-need-action-face))
          (darcsum-filename-face    fs-file)
          (darcsum-change-line-face diff-changed)

          ;; ** emacs-wiki-colors
          (emacs-wiki-bad-link-face (warning emacs-wiki-link-face))
          (emacs-wiki-header-1      (level-1 emacs-wiki-header))
          (emacs-wiki-header-2      (level-2 emacs-wiki-header))
          (emacs-wiki-header-3      (level-3 emacs-wiki-header))
          (emacs-wiki-header-4      (level-4 emacs-wiki-header))
          (emacs-wiki-header-5      (level-5 emacs-wiki-header))
          (emacs-wiki-link-face     (link))
          (emacs-wiki-verbatim-face text-verbatim)

          ;; ** ensime
          (ensime-breakpoint-face         breakpoint-enabled)
          (ensime-implicit-highlight      font-lock-warning-face)
          (ensime-marker-face             hl-line)
          (ensime-pending-breakpoint-face breakpoint-disabled)

          ;; ** envrc
          ;; (envrc-mode-line-error-face (error)) ; already correct
          ;; no .envrc shouldn’t be a warning
          (envrc-mode-line-none-face  (shadow))
          ;; (envrc-mode-line-on-face    (success)) ; already correct

          ;; ** flycheck
          (flycheck-error   error)
          (flycheck-info    message)
          (flycheck-warning warning)

          ;; ** flymake
          (flymake-errline  error)
          (flymake-warnline warning)

          ;; ** flyspell
          (flyspell-duplicate warning)
          (flyspell-incorrect error)

          ;; ** font-latex
          (font-latex-warning-face (warning))

          ;; ** forge
          (forge-post-author (text-author))

          ;; ** git-commit
          (git-commit-comment-file         (vc-dir-file))
          (git-commit-known-pseudo-header  (log-edit-header))
          (git-commit-nonempty-second-line (warning))
          (git-commit-overlong-summary     (warning git-commit-summary))
          (git-commit-pseudo-header        (log-edit-unknown-header))
          (git-commit-summary              (log-edit-summary))

          ;; ** guide-key
          ;; (guide-key/prefix-command-face    )
          (guide-key/highlight-command-face
           (highlight guide-key/prefix-command-face))
          (guide-key/key-face               (help-key-binding))

          ;; ** haskell-mode
          (haskell-error-face    (font-lock-warning-face))
          (haskell-keyword-face  (font-lock-keyword-face))
          (haskell-operator-face (font-lock-operator-face))
          (haskell-warning-face  (font-lock-warning-face))

          ;; ** helm
          (helm-bookmark-directory          (helm-buffer-directory))
          (helm-bookmark-file               (helm-buffer-file))
          (helm-buffer-archive              (helm-buffer-file))
          (helm-buffer-directory            (fs-directory))
          (helm-buffer-file                 (fs-file))
          (helm-buffer-process              (helm-non-file-buffer))
          (helm-command-active-mode         ())
          (helm-etags+-highlight-face       (highlight))
          (helm-ff-backup-file              (shadow helm-ff-file))
          (helm-ff-directory                (helm-buffer-directory))
          (helm-ff-dotted-directory         (helm-ff-directory))
          (helm-ff-dotted-symlink-directory
           (helm-ff-symlink helm-ff-dotted-directory))
          (helm-ff-executable               (fs-executable helm-ff-file))
          (helm-ff-file                     (helm-buffer-file))
          (helm-ff-file-extension           (helm-ff-file))
          (helm-ff-invalid-symlink          (fs-broken-symlink helm-ff-symlink))
          (helm-ff-nofile                   (helm-non-file-buffer))
          (helm-ff-pipe                     (helm-ff-file))
          (helm-ff-socket                   (helm-ff-file))
          (helm-ff-symlink                  (fs-symlink))
          ;; (helm-header) ; Already correct
          ;; (helm-header-line-left-margin     ())
          (helm-lisp-show-completion        (highlight))
          (helm-M-x-key                     (help-key-binding))
          (helm-match                       (match))
          (helm-gentoo-match                (helm-match))
          (helm-grep-match                  (helm-match))
          (helm-selection                   (region))
          (helm-selection-line              (secondary-selection))
          (helm-separator                   (completions-group-separator))
          (helm-source-header               (completions-group-title))
          (helm-w3m-bookmarks               (helm-bookmark-w3m))

          ;;; ** helm-eshell
          (helm-eshell-prompts-buffer-name (green)) ; FIXME aligning original
          (helm-eshell-prompts-promptidx   (cyan)) ; FIXME aligning original

          ;; ** helm-locate
          (helm-locate-finish (success))

          ;; ** helm-ls-git
          (helm-ls-git-added-copied-face        (vc-locally-added-state))
          (helm-ls-git-added-modified-face      (vc-dir-status-edited))
          (helm-ls-git-conflict-face            (vc-conflict-state))
          (helm-ls-git-deleted-and-staged-face  (vc-removed-state))
          (helm-ls-git-deleted-not-staged-face  (vc-removed-state))
          (helm-ls-git-modified-and-staged-face (vc-dir-status-edited))
          (helm-ls-git-modified-not-staged-face (vc-dir-status-edited))
          (helm-ls-git-renamed-modified-face    (vc-dir-status-edited))

          ;; ** highlight-doxygen
          (highlight-doxygen-code-block font-lock)
          (highlight-doxygen-comment    font-lock-doc-face)

          ;; ** hydra
          ;; TODO: These faces shouldn’t be named by color
          (hydra-face-teal     green)   ; Approximately cmyk(f007)
          (hydra-face-blue     cyan)    ; Approximately cmyk(ff00)
          (hydra-face-pink     blue)    ; Approximately cmyk(0302)
          (hydra-face-amaranth magenta) ; Approximately cmyk(0c91)
          (hydra-face-red      red)     ; Approximately cmyk(0ff0)

          ;; ** idris-mode
          (idris-active-term-face       isearch)
          (idris-colon-face             font-lock-builtin-face)
          (idris-definition-face
           (sem-hi-binding
            sem-hi-scope-global
            font-lock-function-name-face))
          (idris-equals-face            font-lock-builtin-face)
          (idris-identifier-face        font-lock-identifier-face)
          (idris-ipkg-keyword-face      font-lock-keyword-face)
          (idris-ipkg-package-face      font-lock-module-face)
          (idris-keyword-face           font-lock-keyword-face)
          (idris-loaded-region-face     highlight)
          (idris-log-level-1-face   (urgency-urgent idris-log-level-face))
          (idris-log-level-2-face     (urgency-high idris-log-level-face))
          (idris-log-level-3-face (urgency-moderate idris-log-level-face))
          (idris-log-level-4-face   (urgency-normal idris-log-level-face))
          (idris-log-level-5-face      (urgency-low idris-log-level-face))
          (idris-log-level-higher-face
           (urgency-trivial idris-log-level-face))
          (idris-metavariable-face      font-lock-variable-name-face)
          (idris-module-face            font-lock-module-face)
          (idris-operator-face          font-lock-operator-face)
          (idris-parameter-face         font-lock-variable-name-face)
          (idris-repl-input-face        input)
          (idris-repl-output-face       output)
          (idris-repl-prompt-face       prompt)
          (idris-repl-result-face       result)
          (idris-semantic-bound-face    sem-hi-bound)
          (idris-semantic-data-face     font-lock-literal-face)
          (idris-semantic-function-face font-lock-function-name-face)
          (idris-semantic-implicit-face font-lock-variable-name-face)
          (idris-semantic-type-face     font-lock-type-face)
          (idris-unsafe-face            font-lock-warning-face)
          (idris-warning-face           warning)

          ;; ** js2-mode
          (js2-external-variable-face        font-lock-warning-face)
          (js2-jsdoc-html-tag-delimiter-face delimiter)
          (js2-jsdoc-html-tag-name-face      font-lock-function-name-face)
          (js2-jsdoc-tag-face                text-definition-term)
          (js2-jsdoc-type-face               font-lock-type-face)
          (js2-jsdoc-value-face              text-definition-explanation)

          ;; ** lsp-headerline
          (lsp-headerline-breadcrumb-project-prefix-face (header-line))
          (lsp-headerline-breadcrumb-unknown-project-prefix-face
           (lsp-headerline-breadcrumb-project-prefix-face))
          (lsp-headerline-breadcrumb-path-face           (header-line))
          (lsp-headerline-breadcrumb-path-error-face
           (error lsp-headerline-breadcrumb-path-face))
          (lsp-headerline-breadcrumb-path-hint-face
           (lsp-headerline-breadcrumb-path-face))
          (lsp-headerline-breadcrumb-path-info-face
           (lsp-headerline-breadcrumb-path-face))
          (lsp-headerline-breadcrumb-path-warning-face
           (warning lsp-headerline-breadcrumb-path-face))
          (lsp-headerline-breadcrumb-symbols-face        (header-line))
          (lsp-headerline-breadcrumb-symbols-error-face
           (error lsp-headerline-breadcrumb-symbols-face))
          (lsp-headerline-breadcrumb-symbols-hint-face
           (lsp-headerline-breadcrumb-symbols-face))
          (lsp-headerline-breadcrumb-symbols-info-face
           (lsp-headerline-breadcrumb-symbols-face))
          (lsp-headerline-breadcrumb-symbols-warning-face
           (warning lsp-headerline-breadcrumb-symbols-face))

          ;; ** lsp-mode
          (lsp-face-highlight-read  (highlight)) ; NB: to drop underline
          ;; (lsp-face-highlight-textual (highlight)) ; already correct
          (lsp-face-highlight-write (highlight)) ; NB: to drop bold

          ;; ** lua2-mode
          (lua2-error error)
          (lua2-bind-variable
           (sem-hi-binding
            sem-hi-scope-local
            font-lock-variable-name-face))
          (lua2-reference-variable
           (sem-hi-reference
            sem-hi-scope-local
            font-lock-variable-name-face))
          (lua2-assign-variable
           (sem-hi-mutable
            sem-hi-scope-local
            font-lock-variable-name-face))
          ;; NB: This one should probably use name hashing
          (lua2-reference-global-variable
           (sem-hi-reference
            sem-hi-scope-global
            font-lock-variable-name-face))
          (lua2-assign-global-variable
           (sem-hi-mutable
            sem-hi-scope-global
            font-lock-variable-name-face))

          ;; ** magit
          (magit-blame-highlight              (highlight))
          (magit-branch-current               (highlight magit-branch-local))
          (magit-branch-remote-head           (highlight magit-branch-remote))
          (magit-branch-upstream              (magit-branch-remote))
          (magit-diff-added                   (diff-added))
          (magit-diff-added-highlight
           (magit-diff-added magit-section-highlight))
          (magit-diff-base                    (diff-changed))
          (magit-diff-base-highlight (magit-diff-base magit-section-highlight))
          (magit-diff-conflict-heading        smerge-markers)
          (magit-diff-context                 (diff-context))
          (magit-diff-context-highlight
           (magit-diff-context magit-section-highlight))
          (magit-diff-diffstat-added          diff-indicator-added)
          (magit-diff-diffstat-removed        diff-indicator-removed)
          (magit-diff-file-heading            diff-file-header)
          (magit-diff-hunk-heading            diff-hunk-header)
          ;; NB: This layers on ‘magit-diff-hunk-heading’, so don’t also inherit
          ;;     that.
          (magit-diff-hunk-heading-highlight  magit-section-highlight)
          (magit-diff-hunk-heading-selection  selection)
          (magit-diff-removed                 (diff-removed))
          (magit-diff-removed-highlight
           (magit-diff-removed magit-section-highlight))
          (magit-diff-revision-summary        text-title)
          (magit-diff-revision-highlight      magit-section-highlight)
          (magit-dimmed                       (shadow))
          ;; NB: This often affects alignment of the ASCII graph
          (magit-hash                         pseudo-column)
          (magit-header-line                  (header-line))
          (magit-header-line-key              (magit-header-line))
          (magit-header-line-log-select       (magit-header-line))
          (magit-key-mode-button-face         button)
          (magit-key-mode-header-face         text-heading)
          (magit-log-author                   (text-author))
          ;; NB: This is drawn as ASCII art
          (magit-log-graph                    fixed-pitch)
          (magit-log-reflog-label-checkout    magit-log-reflog-label-other)
          (magit-log-reflog-label-cherry-pick
           magit-log-reflog-label-other)
          (magit-log-reflog-label-commit      magit-log-reflog-label-other)
          (magit-log-reflog-label-reset       magit-log-reflog-label-other)
          (magit-log-reflog-label-rebase      magit-log-reflog-label-other)
          (magit-log-reflog-label-remote      magit-log-reflog-label-other)
          (magit-process-ng                   (error magit-section-heading))
          (magit-process-ok                   (success magit-section-heading))
          (magit-section-heading              (level-1))
          (magit-section-highlight            (highlight) (:extend t))
          (magit-signature-bad                (error))
          (magit-signature-error              (error))
          (magit-signature-expired            (warning))
          (magit-signature-expired-key        (warning))
          (magit-signature-good               (success))
          (magit-signature-revoked            (warning))
          (magit-signature-untrusted          (warning))

          ;; ** markdown-mode
          (markdown-bold-face             (bold))
          (markdown-comment-face          (font-lock-comment-face))
          (markdown-header-delimiter-face (delimiter))
          (markdown-header-face           (text-heading))
          (markdown-header-face-1         (level-1 markdown-header-face))
          (markdown-header-face-2         (level-2 markdown-header-face))
          (markdown-header-face-3         (level-3 markdown-header-face))
          (markdown-header-face-4         (level-4 markdown-header-face))
          (markdown-header-face-5         (level-5 markdown-header-face))
          (markdown-header-face-6         (level-6 markdown-header-face))
          (markdown-header-rule-face      (shadow))
          (markdown-inline-code-face      (font-lock))
          (markdown-italic-face           (italic))
          (markdown-language-keyword-face (font-lock-keyword-face))
          (markdown-link-face             (link))
          (markdown-link-title-face       ())
          (markdown-metadata-key-face     (text-definition-term))
          (markdown-metadata-value-face   (text-definition-explanation))
          (markdown-pre-face              (text-verbatim))
          (markdown-url-face              (shadow))
          ;; (markdown-plain-url-face (markdown-link-face)) ; already correct

          ;; ** muse
          (muse-bad-link   (warning muse-link))
          (muse-emphasis-1 (italic))
          (muse-emphasis-2 (bold))
          (muse-emphasis-3 (bold-italic))
          (muse-header-1   (level-1 text-heading))
          (muse-header-2   (level-2 text-heading))
          (muse-header-3   (level-3 text-heading))
          (muse-header-4   (level-4 text-heading))
          (muse-header-5   (level-5 text-heading))
          (muse-link       (link))
          (muse-verbatim   (text-verbatim))

          ;; ** paradox
          (paradox-comment-face   (font-lock-comment-face))
          (paradox-highlight-face (highlight))

          ;; ** parenface
          (parenface-bracket-face (shadow))
          (parenface-curly-face   (shadow))
          (parenface-paren-face   (shadow))

          ;; ** paren-face
          (parenthesis (shadow))

          ;; ** popup
          (popup-face ())
          (popup-isearch-match              (match popup-face))
          (popup-scroll-bar-background-face (scroll-bar popup-face))
          (popup-scroll-bar-foreground-face (scroll-bar popup-face))
          (popup-summary-face               (popup-face))
          (popup-tip-face                   (popup-face))

          ;; ** powerline
          (powerline-active1  (mode-line))
          (powerline-active2  (powerline-active1))
          (powerline-inactive1 (inactive-mode-line))
          (powerline-inactive2 (powerline-inactive1))

          ;; ** psvn
          (svn-status-directory-face (fs-directory))
          (svn-status-filename-face  (fs-file))
          (svn-status-symlink-face   (fs-symlink))

          ;; ** rainbow-delimiters
          (rainbow-delimiters-depth-1-face (level-1))
          (rainbow-delimiters-depth-2-face (level-2))
          (rainbow-delimiters-depth-3-face (level-3))
          (rainbow-delimiters-depth-4-face (level-4))
          (rainbow-delimiters-depth-5-face (level-5))
          (rainbow-delimiters-depth-6-face (level-6))
          (rainbow-delimiters-depth-7-face (level-7))
          (rainbow-delimiters-depth-8-face (level-8))
          (rainbow-delimiters-depth-9-face (level-9))

          ;; ** rst-mode
          (rst-level-1 (level-1 text-heading))
          (rst-level-2 (level-2 text-heading))
          (rst-level-3 (level-3 text-heading))
          (rst-level-4 (level-4 text-heading))
          (rst-level-5 (level-5 text-heading))
          (rst-level-6 (level-6 text-heading))

          ;; ** scala-mode2
          (scala-font-lock:abstract-face    (scala-font-lock:keyword-face))
          (scala-font-lock:final-face       (scala-font-lock:keyword-face))
          (scala-font-lock:implicit-face    (scala-font-lock:keyword-face))
          (scala-font-lock:lazy-face        (scala-font-lock:keyword-face))
          (scala-font-lock:override-face    (scala-font-lock:keyword-face))
          (scala-font-lock:private-face     (scala-font-lock:keyword-face))
          (scala-font-lock:protected-face   (scala-font-lock:keyword-face))
          (scala-font-lock:sealed-face      (scala-font-lock:keyword-face))
          (scala-font-lock:var-face         (font-lock-variable-name-face))
          (scala-font-lock:var-keyword-face scala-font-lock:keyword-face)

          ;; ** slime
          (sldb-topline-face                    (slime-topline sldb-default))
          (sldb-condition-face                  (sldb-default))
          (sldb-section-face                    (sldb-default))
          (sldb-frame-label-face                (sldb-default))
          (sldb-restart-face                    (sldb-default))
          (sldb-restart-number-face             (sldb-restart-face))
          (sldb-restart-type-face               (sldb-restart-face))
          (sldb-frame-line-face                 (sldb-default))
          (sldb-restartable-frame-line-face     (sldb-frame-line-face))
          (sldb-non-restartable-frame-line-face (sldb-frame-line-face))
          (sldb-detailed-frame-line-face        (sldb-frame-line-face))
          (sldb-local-name-face                 (sldb-default))
          (sldb-local-value-face                (sldb-default))
          (sldb-catch-tag-face                  (sldb-default))
          (slime-apropos-label                  (slime-apropos-default))
          (slime-apropos-symbol                 (slime-apropos-default))
          (slime-error-face                     (error slime-default))
          (slime-highlight-face                 (highlight slime-default))
          (slime-inspector-topline-face (slime-topline slime-inspector-default))
          (slime-inspector-label-face           (slime-inspector-default))
          (slime-inspector-value-face           (slime-inspector-default))
          (slime-inspector-action-face          (slime-inspector-default))
          (slime-inspector-type-face            (slime-inspector-default))
          (slime-note-face                      (message slime-default))
          (slime-warning-face                   (warning slime-default))
          (slime-style-warning-face             (slime-warning-face))

          ;; ** slime-repl
          (slime-repl-input-face          (input))
          (slime-repl-inputed-output-face (slime-repl-output-face))
          (slime-repl-output-face         (output))
          (slime-repl-prompt-face         (prompt))
          (slime-repl-result-face         (result))

          ;; ** table
          (table-cell (table))

          ;; ** transient
          ;; colors first
          (transient-teal              green)   ; Approximately cmyk(f007)
          (transient-blue              cyan)    ; Approximately cmyk(ff00)
          (transient-purple            blue)    ; Approximately cmyk(0f07)
          (transient-pink              magenta) ; Approximately cmyk(0302)
          (transient-amaranth          red)     ; Approximately cmyk(0c91)
          (transient-red               yellow)  ; Approximately cmyk(0ff0)
          ;; (transient-active-infix      ()) ; already correct
          (transient-argument          (highlight))
          (transient-disabled-suffix   (shadow))
          (transient-enabled-suffix    (success))
          (transient-heading           (level-1))
          ;; (transient-higher-level      ())
          ;; (transient-inactive-argument) ; Already correct
          ;; (transient-inactive-value) ; Already correct
          ;; (transient-inapt-suffix      ())
          ;; See ‘transient-align-variable-pitch’
          (transient-key               (fixed-pitch help-key-binding))
          ;; These two are documented to inherit from ‘transient-key’,
          ;; but they don’t actually.
          (transient-mismatched-key    (warning transient-key))
          (transient-nonstandard-key   (warning transient-key))
          (transient-separator         (mode-line))
          ;; (transient-unreachable) ; Already correct
          (transient-unreachable-key   (transient-unreachable transient-key))
          ;; (transient-value             ())

          ;; ** which-key
          (which-key-command-description-face   (which-key-description-face))
          (which-key-group-description-face     (which-key-description-face))
          (which-key-local-map-description-face (which-key-description-face))
          (which-key-highlighted-command-face
           (highlight which-key-command-description-face))
          (which-key-key-face                   (help-key-binding))
          (which-key-note-face                  (font-lock-doc-face))
          (which-key-separator-face             (delimiter))
          (which-key-specal-key-face            (which-key-key-face))

          ;; ** whitespace
          (whitespace-space            whitespace-default)
          (whitespace-hspace           whitespace-default)
          (whitespace-tab              whitespace-default)
          (whitespace-newline          whitespace-default)
          (whitespace-trailing         (trailing-whitespace whitespace-default))
          (whitespace-line             whitespace-default)
          (whitespace-space-before-tab whitespace-default)
          (whitespace-indentation      whitespace-default)
          (whitespace-empty            whitespace-default)
          (whitespace-space            whitespace-default)
          (whitespace-space-after-tab  whitespace-default)

          ;; ** write-good
          (writegood-duplicates-face    (warning))
          (writegood-passive-voice-face (warning))
          (writegood-weasels-face       (warning)))))

(custom-theme-set-variables
 'inheritance
 '(compilation-message-face 'message)
 '(locate-header-face '(level-1 text-heading)))

(provide-theme 'inheritance)
;;; inheritance-theme.el ends here
