;;; default-theme.el --- Initial faces as a theme -*- lexical-binding: t; -*-

;;; Commentary:

;; This theme should look identical to Emacs’ default configuration. This is
;; useful for creating a theme that is meant as a layer on top of the default,
;; and also means that the default theming doesn’t need to be handled in a
;; special way.

;;; Code:

(deftheme default
  "This is all of Emacs’ default face definitions extracted into a theme.")

(custom-theme-set-faces
 'default
 '(default ((t nil)))
 '(shadow ((((class color grayscale) (min-colors 88) (background light))
            :foreground "grey50")
           (((class color grayscale) (min-colors 88) (background dark))
            :foreground "grey70")
           (((class color) (min-colors 8) (background light))
            :foreground "green")
           (((class color) (min-colors 8) (background dark))
            :foreground "yellow")))
 '(link ((((class color) (min-colors 88) (background light))
          :foreground "RoyalBlue3" :underline t)
         (((class color) (background light))
          :foreground "blue" :underline t)
         (((class color) (min-colors 88) (background dark))
          :foreground "cyan1" :underline t)
         (((class color) (background dark))
          :foreground "cyan" :underline t)
         (t :inherit underline)))
 '(link-visited ((((class color) (background light)) :foreground "magenta4")
                 (((class color) (background dark)) :foreground "violet")))
 '(highlight ((((class color) (min-colors 88) (background light))
               :background "darkseagreen2")
              (((class color) (min-colors 88) (background dark))
               :background "darkolivegreen")
              (((class color) (min-colors 16) (background light))
               :background "darkseagreen2")
              (((class color) (min-colors 16) (background dark))
               :background "darkolivegreen")
              (((class color) (min-colors 8))
               :background "green" :foreground "black")
              (t :inverse-video t)))
 '(region ((((class color) (min-colors 88) (background dark))
            :background "blue3")
           (((class color) (min-colors 88) (background light) (type gtk))
            :distant-foreground "gtk_selection_fg_color"
            :background "gtk_selection_bg_color")
           (((class color) (min-colors 88) (background light) (type ns))
            :distant-foreground "ns_selection_fg_color"
            :background "ns_selection_bg_color")
           (((class color) (min-colors 88) (background light))
            :background "lightgoldenrod2")
           (((class color) (min-colors 16) (background dark))
            :background "blue3")
           (((class color) (min-colors 16) (background light))
            :background "lightgoldenrod2")
           (((class color) (min-colors 8))
            :background "blue" :foreground "white")
           (((type tty) (class mono))
            :inverse-video t)
           (t :background "gray")))
 '(secondary-selection ((((class color) (min-colors 88) (background light))
                         :background "yellow1")
                        (((class color) (min-colors 88) (background dark))
                         :background "SkyBlue4")
                        (((class color) (min-colors 16) (background light))
                         :background "yellow")
                        (((class color) (min-colors 16) (background dark))
                         :background "SkyBlue4")
                        (((class color) (min-colors 8))
                         :background "cyan" :foreground "black")
                        (t :inverse-video t)))
 '(trailing-whitespace ((((class color) (background light))
                         :background "red1")
                        (((class color) (background dark))
                         :background "red1")
                        (t :inverse-video t)))
 '(escape-glyph ((((background dark)) :foreground "cyan")
                 ;; See the comment in minibuffer-prompt for
                 ;; the reason not to use blue on MS-DOS.
                 (((type pc)) :foreground "magenta")
                 ;; red4 is too dark, but some say blue is too loud.
                 ;; brown seems to work ok. -- rms.
                 (t :foreground "brown")))
 '(nobreak-space ((((class color) (min-colors 88))
                   :inherit escape-glyph :underline t)
                  (((class color) (min-colors 8)) :background "magenta")
                  (t :inverse-video t)))
 '(mode-line ((((class color) (min-colors 88))
               :inherit escape-glyph
               :underline t)
              (((class color) (min-colors 8)) :background "magenta")
              (t :inverse-video t)))
 '(mode-line-inactive
   ((((class color) (min-colors 88) (background light))
     :weight light
     :box (:line-width -1 :color "grey75" :style nil)
     :foreground "grey20" :background "grey90")
    (((class color) (min-colors 88) (background dark) )
     :weight light
     :box (:line-width -1 :color "grey40" :style nil)
     :foreground "grey80" :background "grey30")))
 '(mode-line-highlight
   ((((class color) (min-colors 88))
     :box (:line-width 2 :color "grey40" :style released-button))
    (t
     :inherit highlight)))
 '(mode-line-emphasis
   ((t (:weight bold))))

 '(mode-line-buffer-id
   ((t (:weight bold))))

 '(header-line
   ((default :inherit mode-line)
    (((type tty))
     ;; This used to be `:inverse-video t', but that doesn't look very good when
     ;; combined with inverse-video mode-lines and multiple windows. Underlining
     ;; looks better, and is more consistent with the window-system face
     ;; variants, which deemphasize the header-line in relation to the mode-line
     ;; face. If a terminal can't underline, then the header-line will end up
     ;; without any highlighting; this may be too confusing in general, although
     ;; it happens to look good with the only current use of header-lines, the
     ;; info browser. XXX
     :inverse-video nil	       ;Override the value inherited from mode-line.
     :underline t)
    (((class color grayscale) (background light))
     :background "grey90" :foreground "grey20"
     :box nil)
    (((class color grayscale) (background dark))
     :background "grey20" :foreground "grey90"
     :box nil)
    (((class mono) (background light))
     :background "white" :foreground "black"
     :inverse-video nil
     :box nil
     :underline t)
    (((class mono) (background dark))
     :background "black" :foreground "white"
     :inverse-video nil
     :box nil
     :underline t)))

 '(vertical-border
   ((((type tty)) :inherit mode-line-inactive)))

 '(window-divider ((t :foreground "gray60")))

 '(window-divider-first-pixel
   ((t :foreground "gray80")))

 '(window-divider-last-pixel
   ((t :foreground "gray40")))

 '(minibuffer-prompt
   ((((background dark)) :foreground "cyan")
    ;; Don't use blue because many users of the MS-DOS port customize their
    ;; foreground color to be blue.
    (((type pc)) :foreground "magenta")
    (t :foreground "medium blue")))

 '(fringe
   ((((class color) (background light)) :background "grey95")
    (((class color) (background dark)) :background "grey10")
    (t :background "gray")))

 '(scroll-bar ((t nil)))

 '(border ((t nil)))

 '(cursor
   ((((background light)) :background "black")
    (((background dark))  :background "white")))

 '(mouse ((t nil)))

 '(tool-bar
   ((default :box (:line-width 1 :style released-button) :foreground "black")
    (((type x w32 mac ns) (class color)) :background "grey75")
    (((type x) (class mono)) :background "grey")))

 '(menu
   ((((type tty)) :inverse-video t)
    (((type x-toolkit)))
    (t :inverse-video t)))

 '(help-argument-name ((t :inherit italic)))

 '(glyphless-char
   ((((type tty)) :inherit underline)
    (((type pc)) :inherit escape-glyph)
    (t :height 0.6)))

 '(error
   ((default :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "Red1")
    (((class color) (min-colors 88) (background dark))  :foreground "Pink")
    (((class color) (min-colors 16) (background light)) :foreground "Red1")
    (((class color) (min-colors 16) (background dark))  :foreground "Pink")
    (((class color) (min-colors 8)) :foreground "red")
    (t :inverse-video t)))

 '(warning
   ((default :weight bold)
    (((class color) (min-colors 16)) :foreground "DarkOrange")
    (((class color)) :foreground "yellow")))

 '(success
   ((default :weight bold)
    (((class color) (min-colors 16) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark)) :foreground "Green1")
    (((class color) (min-colors 16) (background dark)) :foreground "Green")
    (((class color)) :foreground "green")))

 ;; Faces for TTY menus.
 '(tty-menu-enabled-face
   ((t :foreground "yellow" :background "blue" :weight bold)))

 '(tty-menu-disabled-face
   ((((class color) (min-colors 16)) :foreground "lightgray" :background "blue")
    (t :foreground "white" :background "blue")))

 '(tty-menu-selected-face
   ((t :background "red")))

 '(show-paren-match
   (;; looks OK on tty (becomes cyan)
    (((class color) (background light)) :background "turquoise")
    ;; looks OK on tty (becomes blue)
    (((class color) (background dark)) :background "steelblue3")
    (((background dark)) :background "grey50")
    (t :background "gray")))

 '(show-paren-mismatch
   ((((class color)) (:foreground "white" :background "purple"))
    (t (:inverse-video t)))))

(provide-theme 'default)
;;; default-theme.el ends here
