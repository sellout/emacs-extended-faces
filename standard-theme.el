;;; standard-theme.el --- Initial faces as a theme -*- lexical-binding: t; -*-

;;; Commentary:

;; This theme should look identical to Emacs’ standard configuration. This is
;; useful for creating a theme that is meant as a layer on top of the standard,
;; and also means that the standard theming doesn’t need to be handled in a
;; special way.

;;; Code:

(deftheme standard
  "This is all of Emacs’ standard face definitions extracted into a theme.")

(custom-theme-set-faces
 'standard
 '(default
   ((t nil)))
 '(bold
   ((t (:weight bold))))
 '(italic
   ((((supports :slant italic)) (:slant italic))
    (((supports :underline t)) (:underline (:color foreground-color :style line)))
    (t (:slant italic))))
 '(bold-italic
   ((t (:slant italic :weight bold))))
 '(underline
   ((((supports :underline t)) (:underline (:color foreground-color :style line)))
    (((supports :weight bold)) (:weight bold))
    (t (:underline (:color foreground-color :style line)))))
 '(fixed-pitch
   ((t (:family "Monospace"))))
 '(fixed-pitch-serif
   ((t (:family "Monospace Serif"))))
 '(variable-pitch
   ((((type w32)) (:foundry "outline" :family "Arial"))
    (t (:family "Sans Serif"))))
 '(shadow
   ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50"))
    (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70"))
    (((class color) (min-colors 8) (background light)) (:foreground "green"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(link
   ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3"))
    (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue"))
    (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1"))
    (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan"))
    (t (:inherit (underline)))))
 '(link-visited
   ((default (:inherit (link)))
    (((class color) (background light)) (:foreground "magenta4"))
    (((class color) (background dark)) (:foreground "violet"))))
 '(highlight
   ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2"))
    (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen"))
    (((class color) (min-colors 16) (background light)) (:background "darkseagreen2"))
    (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen"))
    (((class color) (min-colors 8)) (:foreground "black" :background "green"))
    (t (:inverse-video t))))
 '(region
   ((((class color) (min-colors 88) (background dark)) (:extend t :background "blue3"))
    (((class color) (min-colors 88) (background light) (type gtk)) (:extend t :background "gtk_selection_bg_color" :distant-foreground "gtk_selection_fg_color"))
    (((class color) (min-colors 88) (background light) (type ns)) (:extend t :background "ns_selection_bg_color" :distant-foreground "ns_selection_fg_color"))
    (((class color) (min-colors 88) (background light)) (:extend t :background "lightgoldenrod2"))
    (((class color) (min-colors 16) (background dark)) (:extend t :background "blue3"))
    (((class color) (min-colors 16) (background light)) (:extend t :background "lightgoldenrod2"))
    (((class color) (min-colors 8)) (:extend t :foreground "white" :background "blue"))
    (((type tty) (class mono)) (:inverse-video t))
    (t (:extend t :background "gray"))))
 '(secondary-selection
   ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1"))
    (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4"))
    (((class color) (min-colors 16) (background light)) (:extend t :background "yellow"))
    (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4"))
    (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan"))
    (t (:inverse-video t))))
 '(trailing-whitespace
   ((((class color) (background light)) (:background "red1"))
    (((class color) (background dark)) (:background "red1"))
    (t (:inverse-video t))))
 '(line-number
   ((t (:inherit (shadow default)))))
 '(line-number-current-line
   ((t (:inherit (line-number)))))
 '(line-number-major-tick
   ((((class color grayscale) (background light)) (:bold t :background "grey85"))
    (((class color grayscale) (background dark)) (:bold t :background "grey75"))
    (t (:inherit (line-number)))))
 '(line-number-minor-tick
   ((((class color grayscale) (background light)) (:bold t :background "grey95"))
    (((class color grayscale) (background dark)) (:bold t :background "grey55"))
    (t (:inherit (line-number)))))
 '(fill-column-indicator
   ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :inherit (shadow)))))
 '(escape-glyph
   ((((background dark)) (:foreground "cyan"))
    (((type pc)) (:foreground "magenta"))
    (t (:foreground "brown"))))
 '(homoglyph
   ((((background dark)) (:foreground "cyan"))
    (((type pc)) (:foreground "magenta"))
    (t (:foreground "brown"))))
 '(nobreak-space
   ((((class color) (min-colors 88)) (:underline (:color foreground-color :style line) :inherit (escape-glyph)))
    (((class color) (min-colors 8)) (:background "magenta"))
    (t (:inverse-video t))))
 '(nobreak-hyphen
   ((((background dark)) (:foreground "cyan"))
    (((type pc)) (:foreground "magenta"))
    (t (:foreground "brown"))))
 '(mode-line
   ((((class color) (min-colors 88)) (:foreground "black" :background "grey75" :box (:line-width (1 . -1) :color nil :style released-button)))
    (t (:inverse-video t))))
 '(mode-line-inactive
   ((default (:inherit (mode-line)))
    (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width (1 . -1) :color "grey75" :style nil) :weight light))
    (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width (1 . -1) :color "grey40" :style nil) :weight light))))
 '(mode-line-highlight
   ((((supports :box t) (class color) (min-colors 88)) (:box (:line-width (2 . 2) :color "grey40" :style released-button)))
    (t (:inherit (highlight)))))
 '(mode-line-emphasis
   ((t (:weight bold))))
 '(mode-line-buffer-id
   ((t (:weight bold))))
 '(header-line
   ((default (:inherit (mode-line)))
    (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil))
    (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90"))
    (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20"))
    (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white"))
    (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
 '(header-line-highlight
   ((t (:inherit (mode-line-highlight)))))
 '(vertical-border
   ((((type tty)) (:inherit (mode-line-inactive)))))
 '(window-divider
   ((t (:foreground "gray60"))))
 '(window-divider-first-pixel
   ((t (:foreground "gray80"))))
 '(window-divider-last-pixel
   ((t (:foreground "gray40"))))
 '(internal-border
   ((t nil)))
 '(child-frame-border
   ((t nil)))
 '(minibuffer-prompt
   ((((background dark)) (:foreground "cyan"))
    (((type pc)) (:foreground "magenta"))
    (t (:foreground "medium blue"))))
 '(fringe
   ((((class color) (background light)) (:background "grey95"))
    (((class color) (background dark)) (:background "grey10"))
    (t (:background "gray"))))
 '(scroll-bar
   ((((background light)) (:foreground "black"))
    (((background dark)) (:foreground "white"))))
 '(border
   ((t nil)))
 '(cursor
   ((((background light)) (:background "black"))
    (((background dark)) (:background "white"))))
 '(mouse
   ((t nil)))
 '(tool-bar
   ((default (:foreground "black" :box (:line-width (1 . 1) :color nil :style released-button)))
    (((type x w32 ns) (class color)) (:background "grey75"))
    (((type x) (class mono)) (:background "grey"))))
 '(tab-bar
   ((((class color) (min-colors 88)) (:foreground "black" :background "grey85" :inherit (variable-pitch)))
    (((class mono)) (:background "grey"))
    (t (:inverse-video t))))
 '(tab-line
   ((((class color) (min-colors 88)) (:foreground "black" :background "grey85" :height 0.9 :inherit (variable-pitch)))
    (((class mono)) (:background "grey"))
    (t (:inverse-video t))))
 '(menu
   ((((type tty)) (:inverse-video t))
    (((type x-toolkit)) nil)
    (t (:inverse-video t))))
 '(help-argument-name
   ((t (:inherit (italic)))))
 '(help-key-binding
   ((((class color) (min-colors 88) (background light)) (:box (:line-width (1 . -1) :color "grey80" :style nil) :foreground "DarkBlue" :background "grey96"))
    (((class color) (min-colors 88) (background dark)) (:box (:line-width (1 . -1) :color "grey35" :style nil) :foreground "LightBlue" :background "grey19"))
    (((class color grayscale) (background light)) (:background "grey90"))
    (((class color grayscale) (background dark)) (:background "grey25"))
    (t (:background "grey90"))))
 '(glyphless-char
   ((((type tty)) (:inherit (underline)))
    (((type pc)) (:inherit (escape-glyph)))
    (t (:height 0.6))))
 '(error
   ((default (:weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Red1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Pink"))
    (((class color) (min-colors 16) (background light)) (:foreground "Red1"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Pink"))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t))))
 '(warning
   ((default (:weight bold))
    (((class color) (min-colors 16)) (:foreground "DarkOrange"))
    (((class color)) (:foreground "yellow"))))
 '(success
   ((default (:weight bold))
    (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Green1"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Green"))
    (((class color)) (:foreground "green"))))
 '(read-multiple-choice-face
   ((t (:weight bold :inherit (underline)))))
 '(tty-menu-enabled-face
   ((((class color)) (:weight bold :background "blue" :foreground "yellow"))
    (t (:weight bold))))
 '(tty-menu-disabled-face
   ((((class color) (min-colors 16)) (:background "blue" :foreground "lightgray"))
    (((class color)) (:background "blue" :foreground "white"))
    (t (:inherit (shadow)))))
 '(tty-menu-selected-face
   ((((class color)) (:background "red"))
    (t (:inverse-video t))))
 '(show-paren-match
   ((((class color) (background light)) (:background "turquoise"))
    (((class color) (background dark)) (:background "steelblue3"))
    (((background dark) (min-colors 4)) (:background "grey50"))
    (((background light) (min-colors 4)) (:background "gray"))
    (t (:inherit (underline)))))
 '(show-paren-match-expression
   ((t (:inherit (show-paren-match)))))
 '(show-paren-mismatch
   ((((class color)) (:background "purple" :foreground "white"))
    (t (:inverse-video t))))
 '(button
   ((t (:inherit (link)))))
 '(next-error
   ((t (:inherit (region)))))
 '(next-error-message
   ((t (:extend t :inherit (highlight)))))
 '(separator-line
   ((((type graphic) (background dark)) (:background "#505050" :height 0.1))
    (((type graphic) (background light)) (:background "#a0a0a0" :height 0.1))
    (t (:foreground "ForestGreen"))))
 '(help-for-help-header
   ((t (:height 1.26))))
 '(completions-group-title
   ((t (:slant italic :inherit (shadow)))))
 '(completions-group-separator
   ((t (:strike-through t :inherit (shadow)))))
 '(completions-annotations
   ((t (:inherit (italic shadow)))))
 '(completions-first-difference
   ((t (:inherit (bold)))))
 '(completions-common-part
   ((((class color) (min-colors 16) (background light)) (:foreground "blue3"))
    (((class color) (min-colors 16) (background dark)) (:foreground "lightblue"))))
 '(font-lock-comment-face
   ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "DimGray"))
    (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "LightGray"))
    (((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light)) (:foreground "red"))
    (((class color) (min-colors 16) (background dark)) (:foreground "red1"))
    (((class color) (min-colors 8) (background light)) (:foreground "red"))
    (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))
    (t (:slant italic :weight bold))))
 '(font-lock-comment-delimiter-face
   ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-string-face
   ((((class grayscale) (background light)) (:slant italic :foreground "DimGray"))
    (((class grayscale) (background dark)) (:slant italic :foreground "LightGray"))
    (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic))))
 '(font-lock-doc-face
   ((t (:inherit (font-lock-string-face)))))
 '(font-lock-doc-markup-face
   ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-keyword-face
   ((((class grayscale) (background light)) (:weight bold :foreground "LightGray"))
    (((class grayscale) (background dark)) (:weight bold :foreground "DimGray"))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:weight bold :foreground "cyan"))
    (t (:weight bold))))
 '(font-lock-builtin-face
   ((((class grayscale) (background light)) (:weight bold :foreground "LightGray"))
    (((class grayscale) (background dark)) (:weight bold :foreground "DimGray"))
    (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
    (((class color) (min-colors 8)) (:weight bold :foreground "blue"))
    (t (:weight bold))))
 '(font-lock-function-name-face
   ((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
    (((class color) (min-colors 8)) (:weight bold :foreground "blue"))
    (t (:weight bold :inverse-video t))))
 '(font-lock-variable-name-face
   ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90"))
    (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray"))
    (((class color) (min-colors 88) (background light)) (:foreground "sienna"))
    (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
    (((class color) (min-colors 8)) (:weight light :foreground "yellow"))
    (t (:slant italic :weight bold))))
 '(font-lock-type-face
   ((((class grayscale) (background light)) (:weight bold :foreground "Gray90"))
    (((class grayscale) (background dark)) (:weight bold :foreground "DimGray"))
    (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-constant-face
   ((((class grayscale) (background light)) (:underline (:color foreground-color :style line) :weight bold :foreground "LightGray"))
    (((class grayscale) (background dark)) (:underline (:color foreground-color :style line) :weight bold :foreground "Gray50"))
    (((class color) (min-colors 88) (background light)) (:foreground "dark cyan"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-warning-face
   ((t (:inherit (error)))))
 '(font-lock-negation-char-face
   ((t nil)))
 '(font-lock-preprocessor-face
   ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash
   ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct
   ((t (:inherit (bold)))))
 '(mouse-drag-and-drop-region
   ((t (:inherit (region)))))
 '(isearch
   ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3"))
    (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2"))
    (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4"))
    (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4"))
    (t (:inverse-video t))))
 '(isearch-fail
   ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1"))
    (((class color) (min-colors 88) (background dark)) (:background "red4"))
    (((class color) (min-colors 16)) (:background "red"))
    (((class color) (min-colors 8)) (:background "red"))
    (((class color grayscale)) (:foreground "grey"))
    (t (:inverse-video t))))
 '(lazy-highlight
   ((((class color) (min-colors 88) (background light)) (:background "paleturquoise"))
    (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4"))
    (((class color) (min-colors 16)) (:background "turquoise3"))
    (((class color) (min-colors 8)) (:background "turquoise3"))
    (t (:underline (:color foreground-color :style line)))))
 '(isearch-group-1
   ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "#f000f0"))
    (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred1"))
    (t (:inherit (isearch)))))
 '(isearch-group-2
   ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "#a000a0"))
    (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred3"))
    (t (:inherit (isearch)))))
 '(file-name-shadow
   ((t (:inherit (shadow)))))
 '(tab-bar-tab
   ((default (:inherit (tab-bar)))
    (((class color) (min-colors 88)) (:box (:line-width (1 . 1) :color nil :style released-button)))
    (t (:inverse-video nil))))
 '(tab-bar-tab-inactive
   ((default (:inherit (tab-bar-tab)))
    (((class color) (min-colors 88)) (:background "grey75"))
    (t (:inverse-video t))))
 '(tab-bar-tab-group-current
   ((t (:weight bold :box nil :inherit (tab-bar-tab)))))
 '(tab-bar-tab-group-inactive
   ((t (:inherit (shadow tab-bar-tab-inactive)))))
 '(tab-bar-tab-ungrouped
   ((t (:inherit (shadow tab-bar-tab-inactive)))))
 '(query-replace
   ((t (:inherit (isearch)))))
 '(match
   ((((class color) (min-colors 88) (background light)) (:background "khaki1"))
    (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3"))
    (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow"))
    (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue"))
    (((type tty) (class mono)) (:inverse-video t))
    (t (:background "gray"))))
 '(tabulated-list-fake-header
   ((t (:weight bold :underline (:color foreground-color :style line) :overline t))))
 '(buffer-menu-buffer
   ((t (:weight bold))))
 '(ns-working-text-face
   ((t (:underline (:color foreground-color :style line)))))
 '(vc-state-base
   ((default nil)))
 '(vc-up-to-date-state
   ((default (:inherit (vc-state-base)))))
 '(vc-needs-update-state
   ((default (:inherit (vc-state-base)))))
 '(vc-locked-state
   ((default (:inherit (vc-state-base)))))
 '(vc-locally-added-state
   ((default (:inherit (vc-state-base)))))
 '(vc-conflict-state
   ((default (:inherit (vc-state-base)))))
 '(vc-removed-state
   ((default (:inherit (vc-state-base)))))
 '(vc-missing-state
   ((default (:inherit (vc-state-base)))))
 '(vc-edited-state
   ((default (:inherit (vc-state-base)))))
 '(elisp-shorthand-font-lock-face
   ((t (:foreground "cyan" :inherit (font-lock-keyword-face)))))
 '(eldoc-highlight-function-argument
   ((t (:inherit (bold)))))
 '(tooltip
   ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow"))
    (t (:inherit (variable-pitch)))))
 '(widget-documentation
   ((((class color) (background dark)) (:foreground "lime green"))
    (((class color) (background light)) (:foreground "dark green"))
    (t nil)))
 '(widget-button
   ((t (:weight bold))))
 '(widget-field
   ((((type tty)) (:extend t :foreground "black" :background "yellow3"))
    (((class grayscale color) (background light)) (:extend t :background "gray85"))
    (((class grayscale color) (background dark)) (:extend t :background "dim gray"))
    (t (:extend t :slant italic))))
 '(widget-single-line-field
   ((((type tty)) (:foreground "black" :background "green3"))
    (((class grayscale color) (background light)) (:background "gray85"))
    (((class grayscale color) (background dark)) (:background "dim gray"))
    (t (:slant italic))))
 '(widget-inactive
   ((t (:inherit (shadow)))))
 '(widget-button-pressed
   ((((min-colors 88) (class color)) (:foreground "red1"))
    (((class color)) (:foreground "red"))
    (t (:underline (:color foreground-color :style line) :weight bold))))
 '(custom-invalid
   ((((class color)) (:background "red1" :foreground "yellow1"))
    (t (:underline (:color foreground-color :style line) :slant italic :weight bold))))
 '(custom-rogue
   ((((class color)) (:background "black" :foreground "pink"))
    (t (:underline (:color foreground-color :style line)))))
 '(custom-modified
   ((((min-colors 88) (class color)) (:background "blue1" :foreground "white"))
    (((class color)) (:background "blue" :foreground "white"))
    (t (:slant italic))))
 '(custom-set
   ((((min-colors 88) (class color)) (:background "white" :foreground "blue1"))
    (((class color)) (:background "white" :foreground "blue"))
    (t (:slant italic))))
 '(custom-changed
   ((((min-colors 88) (class color)) (:background "blue1" :foreground "white"))
    (((class color)) (:background "blue" :foreground "white"))
    (t (:slant italic))))
 '(custom-themed
   ((((min-colors 88) (class color)) (:background "blue1" :foreground "white"))
    (((class color)) (:background "blue" :foreground "white"))
    (t (:slant italic))))
 '(custom-saved
   ((t (:underline (:color foreground-color :style line)))))
 '(custom-button
   ((((type x w32 ns) (class color)) (:foreground "black" :background "lightgrey" :box (:line-width (2 . 2) :color nil :style released-button)))))
 '(custom-button-mouse
   ((((type x w32 ns) (class color)) (:foreground "black" :background "grey90" :box (:line-width (2 . 2) :color nil :style released-button)))
    (t (:inverse-video t))))
 '(custom-button-unraised
   ((t (:inherit (underline)))))
 '(custom-button-pressed
   ((((type x w32 ns) (class color)) (:foreground "black" :background "lightgrey" :box (:line-width (2 . 2) :color nil :style pressed-button)))
    (t (:inverse-video t))))
 '(custom-button-pressed-unraised
   ((default (:inherit (custom-button-unraised)))
    (((class color) (background light)) (:foreground "magenta4"))
    (((class color) (background dark)) (:foreground "violet"))))
 '(custom-documentation
   ((t nil)))
 '(custom-state
   ((((class color) (background dark)) (:foreground "lime green"))
    (((class color) (background light)) (:foreground "dark green"))))
 '(custom-link
   ((t (:inherit (link)))))
 '(custom-comment
   ((((type tty)) (:foreground "black" :background "yellow3"))
    (((class grayscale color) (background light)) (:background "gray85"))
    (((class grayscale color) (background dark)) (:background "dim gray"))
    (t (:slant italic))))
 '(custom-comment-tag
   ((((class color) (background dark)) (:foreground "gray80"))
    (((class color) (background light)) (:foreground "blue4"))
    (((class grayscale) (background light)) (:slant italic :weight bold :foreground "DimGray"))
    (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "LightGray"))
    (t (:weight bold))))
 '(custom-variable-obsolete
   ((((class color) (background dark)) (:foreground "light blue"))
    (((min-colors 88) (class color) (background light)) (:foreground "blue1"))
    (((class color) (background light)) (:foreground "blue"))
    (t (:slant italic))))
 '(custom-variable-tag
   ((((class color) (background dark)) (:weight bold :foreground "light blue"))
    (((min-colors 88) (class color) (background light)) (:weight bold :foreground "blue1"))
    (((class color) (background light)) (:weight bold :foreground "blue"))
    (t (:weight bold))))
 '(custom-variable-button
   ((t (:weight bold :underline (:color foreground-color :style line)))))
 '(custom-visibility
   ((t (:inherit (link) :height 0.8))))
 '(custom-face-tag
   ((t (:inherit (custom-variable-tag)))))
 '(custom-group-tag-1
   ((default (:inherit (variable-pitch) :height 1.2 :weight bold))
    (((class color) (background dark)) (:foreground "pink"))
    (((min-colors 88) (class color) (background light)) (:foreground "red1"))
    (((class color) (background light)) (:foreground "red"))))
 '(custom-group-tag
   ((default (:inherit (variable-pitch) :height 1.2 :weight bold))
    (((class color) (background dark)) (:foreground "light blue"))
    (((min-colors 88) (class color) (background light)) (:foreground "blue1"))
    (((class color) (background light)) (:foreground "blue"))
    (t (:weight bold))))
 '(custom-group-subtitle
   ((t (:weight bold)))))

(provide-theme 'standard)
;;; standard-theme.el ends here
