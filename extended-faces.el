;;; extended-faces.el --- Additional generic faces  -*- lexical-binding: t; -*-

;; Copyright (C) 2015–2022 Greg Pfeil

;; Author: Greg Pfeil <greg@technomadic.org>
;; Keywords: faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides many new generic faces for Emacs, in order for themes and
;; packages to be able to present more consistently.

;;; Code:

(defmacro ef-defface (face spec doc &rest args)
  "Define a FACE with the given SPEC, DOC, and other ARGS.
Mostly a synonym for ‘defface’, but when this face is injected into a different
group, it indicates in the comment that it was defined in this package."
  (declare (doc-string 3))
  `(defface ,face
     ,spec
     ,(if (eq (plist-get args :group) 'extended-faces)
          doc
        (concat doc " (Injected by extended-faces.)"))
     ,@args))

(ef-defface message ()
  "Extended face that covers all messages – errors, warnings, and successes."
  :group 'extended-faces)

(ef-defface delimiter '((default (:inherit (shadow))))
  "Delimiters between text."
  :group 'extended-faces)

(ef-defface pseudo-column '((default (:inherit (fixed-pitch))))
  "An indicator that ‘fixed-pitch’ is being used to emulate a columnar layout.
This is distinct from using ‘fixed-pitch’ in, say, ‘compilation-mode’ where
there may occassionally be tables or other output that requires ‘fixed-pitch’
alignment.

There are situations in Emacs where you can better approximate actual columns.
E.g., Info node ‘(elisp)Display Margins’ will allow you to have up to three
columns, as long as the middle column is the only one that needs to be
interactive."
  :group 'extended-faces)

(ef-defface table '((default (:inherit (pseudo-column))))
  "Text-based tables within a buffer."
  :group 'extended-faces)

;;; FONT LOCK

(ef-defface font-lock '((default (:inherit (fixed-pitch))))
  "For any programming face"
  :group 'font-lock)

;; TODO: Add something similar for buffers with specific names (e.g.,
;;      ‘which-key-buffer-name’).
(defun ef-default-mode-face (face modes)
  "Set the FACE to treat as ‘default’ for the provided MODES."
  (mapcar (lambda (mode)
            ;; FIXME: How to do this without trampling on customizable
            ;;        variables?
            (add-hook (intern (concat (symbol-name mode) "-hook"))
                      (lambda () (buffer-face-set face))))
          modes))

;;; NB: It would be nice if comint had an output-specific face – I would
;;;     probably leave the buffer ‘default’, set input to ‘font-lock’ and output
;;;     to ‘fixed-pitch’.
(ef-default-mode-face 'fixed-pitch
  '(comint-mode
    compilation-mode
    eshell-mode))

(ef-default-mode-face 'pseudo-column
  '(dired-mode
    ibuffer-mode
    tabulated-list-mode))

;; Alignment is still important in most programming languages.
;;
;; TODO: Maybe set this back to ‘default’ for languages that have less-aligny
;;       formatting styles?
(ef-default-mode-face 'font-lock '(prog-mode))

(ef-defface font-lock-value-face ()
  "For any value-level construct"
  :group 'font-lock)

(ef-defface font-lock-identifier-face '((default (:inherit (font-lock-value-face))))
  "For any value-level construct"
  :group 'font-lock)

(ef-defface font-lock-literal-face '((default (:inherit (font-lock-value-face))))
  "For any value-level construct"
  :group 'font-lock)

(ef-defface font-lock-module-face '((default (:inherit (font-lock-value-face))))
  "For any larger grouping construct – class, package, module, etc."
  :group 'font-lock)

(ef-defface font-lock-operator-face
  '((default (:inherit (font-lock-function-name-face))))
  "For symbolic operators."
  :group 'font-lock)

;;; NEW FACES

;;; generic

;;; Define 11 levels only because that is the number Gnus goes up to.
;;; TODO: Define a var that takes a list of specs (of length n) for assigning
;;;       item m mod n to level-m.
(ef-defface level-1 ()
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-2 '((default (:inherit (level-1))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-3 '((default (:inherit (level-2))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-4 '((default (:inherit (level-3))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-5 '((default (:inherit (level-4))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-6 '((default (:inherit (level-5))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-7 '((default (:inherit (level-6))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-8 '((default (:inherit (level-7))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-9 '((default (:inherit (level-8))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-10 '((default (:inherit (level-9))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)
(ef-defface level-11 '((default (:inherit (level-10))))
  "Accessory face for multi-level things, like outlines.
Inherits from the previous level so you can customize levels 1–n, then n+1 on
will match n."
  :group 'extended-faces)

;;; urgency – used for things like alert and log levels
;;; NB: Consider urgency in the context of the whole system. If you have three
;;;     levels in your mode, perhaps choose moderate/normal/low so that truly
;;;     urgent things in the system can be given more prevalence

(ef-defface urgency-urgent ()
  "Highest urgency."
  :group 'extended-faces)
(ef-defface urgency-high '((default (:inherit (urgency-urgent))))
  "2nd-highest urgency."
  :group 'extended-faces)
(ef-defface urgency-moderate '((default (:inherit (urgency-high))))
  "3rd-highest urgency."
  :group 'extended-faces)
(ef-defface urgency-normal '((default (:inherit (urgency-moderate))))
  "3rd-lowest (4th-highest) urgency."
  :group 'extended-faces)
(ef-defface urgency-low '((default (:inherit (urgency-normal))))
  "2nd-lowest urgency."
  :group 'extended-faces)
(ef-defface urgency-trivial '((default (:inherit (urgency-low))))
  "Lowest urgency."
  :group 'extended-faces)

(ef-defface input ()
  "For input entered by the user."
  :group 'extended-faces)

(ef-defface output ()
  "A face used for output from processes."
  :group 'extended-faces)

(ef-defface prompt ()
  "A face used for any kind of shell-like input prompt."
  :group 'extended-faces)

(ef-defface result '((default (:inherit (output))))
  "A face used for the result from processes."
  :group 'extended-faces)

;;; explicit colors

;; These should be inherited from fairly rarely. This is not about “I want this
;; to be green.” That is a theme decision. This is for integration with things
;; where there is no meaning beyond the color. E.g., ANSI terminals want these
;; colors, but you have no idea what they will be used for.

;; Guidelines for mapping faces named after colors
;;
;; 1. find the CMYK value for each “requested” color name,
;; 2. order them based on their clockwise position on the CMYK color wheel
;;   (generally cyan first, but if there is a significant gap in their
;;    positions, split it there),
;; 3. do a best-fit from the primary & secondary colors below to the color
;;    requested by the face (in general, err toward distinct faces rather than
;;    closest approximation – see ‘hydra’ and ‘transient’ for examples),
;; 4. suggest to the upstream package maintainers that they use semantic faces
;;    instead of color faces.
;;
;; If there are more than 6–8 colors (depending on whether there are reasonable
;; mappings for ‘black’ and ‘white’) you will need to duplicate some of them.

(ef-defface cyan '((default (:foreground "cyan")))
  "Things that are explicitly cyan.
Approximately cmyk(f000). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)
(ef-defface blue '((default (:foreground "blue")))
  "Things that are explicitly blue.
Approximately cmyk(ff00). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)
(ef-defface magenta '((default (:foreground "magenta")))
  "Things that are explicitly magenta.
Approximately cmyk(0f00). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)
(ef-defface red '((default (:foreground "red")))
  "Things that are explicitly red.
Approximately cmyk(0ff0). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)
(ef-defface yellow '((default (:foreground "yellow")))
  "Things that are explicitly yellow.
Approximately cmyk(00f0). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)
(ef-defface green '((default (:foreground "green")))
  "Things that are explicitly green.
Approximately cmyk(f0f0). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)
(ef-defface black '((default (:foreground "black")))
  "Things that are explicitly black.
Approximately cmyk(000f). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)
(ef-defface white '((default (:foreground "white")))
  "Things that are explicitly white.
Approximately cmyk(0000). Should be used rarely, but useful for term colors,
etc."
  :group 'extended-faces)

;;; filesystem

(ef-defface fs-directory () "Things that represent a directory."
  :group 'extended-faces)
(ef-defface fs-executable '((default (:inherit (file))))
  "Things that represent an executable."
  :group 'extended-faces)
(ef-defface fs-file () "Things that represent a file."
  :group 'extended-faces)
(ef-defface fs-broken-symlink '((default (:inherit (warning fs-symlink))))
  "Things that represent a broken symlink."
  :group 'extended-faces)
(ef-defface fs-symlink () "Things that represent a symlink."
  :group 'extended-faces)

;;; text formatting

(ef-defface text ()
  "A face that all text-specific faces inherit from.
This makes it easy to use a different font for text modes vs programming modes."
  :group 'extended-faces)

(ef-defface text-heading '((default (:inherit (text))))
  "A heading.
This is often combined with the ‘level’ faces, and should usually follow the
level in the inheritance list (e.g., '(level-2 text-heading))."
  :group 'extended-faces)

(ef-defface text-definition-term '((default (:inherit (text))))
  "For the term half of a definition in a definition list."
  :group 'extended-faces)

(ef-defface text-definition-explanation '((default (:inherit (text))))
  "For the explanatory half of a definition in a definition list."
  :group 'extended-faces)

(ef-defface text-emphasis '((default (:inherit (italic text))))
  "The face to use for emphasized text (defaults to ‘italic’)."
  :group 'extended-faces)

(ef-defface text-emphasis-strong '((default (:inherit (bold text))))
  "The face to use for strongly-emphasized text (defaults to ‘bold’)."
  :group 'extended-faces)

(ef-defface text-title '((default (:inherit (text))))
  "Document titles in various formats.
Some formats distinguish between titles and header levels (e.g., ‘org-mode’)
while others don’t (‘markdown-mode’). In the latter, this _may_ be used for the
top level, shifting ‘level-1’ to be used for the second-level headers in those
modes."
  :group 'extended-faces)

(ef-defface text-verbatim '((default (:inherit (fixed-pitch text))))
  "The face to use for text that should be treated literally."
  :group 'extended-faces)

(ef-defface button-mouseover '((default (:inherit (button))))
  "Button with the mouse hovering over it."
  :group 'extended-faces)

(ef-defface button-pressed '((default (:inherit (button))))
  "Button when pressed."
  :group 'extended-faces)

;;; standardizing semantic highlighting

;;; one dimension is scope: global/file/local(nestable)/undef/broken(like JS)
;;; another is use: mutated/unmutated/constant/unreferenced
;;; another is binding/reference (and “unreferenced” only makes sense on binding
;;; - in some langs, scope might be redundant with binding, but in others, you
;;;   can declare variables of different scopes in the same place
;;; - undefined and unreferenced can use the same attr, the former only applies
;;;   to references and the latter only to bindings, so the distinction between
;;;   those will distinguish it
;;;
;;; add a dimension for values vs types? what about values vs functions? those
;;; are more different in some languages than in others (can use the standard
;;; font-lock faces, perhaps, but I think the _intent_ of those is really more
;;; var+binding or func+binding, etc. … but maybe not for type?
;;;
;;; - in some envs, you can’t use [refl|introsp]ection to see what globals might
;;;   exist (cf. Lua), so hashing non-local-var names is a reasonable fallback
;;;   in this case (maybe ignore themes and just pick colors that have enough
;;;   contrast vs the ‘sem-hi-global-scope’ background color) but probably still
;;;   want to annotate with global indicator.

(ef-defface sem-hi-scope-global ()
  "A variable with global scope.
NB: Your mode should use this face only when it can statically guarantee the
name is in scope. Otherwise, it should cycle, using name hashes."
  :group 'extended-faces)
(ef-defface sem-hi-scope-file ()
  "A variable with file scope."
  :group 'extended-faces) ;;; do we want to be explicit about file scope??
(ef-defface sem-hi-scope-local ()
  "A variable with local (lexical) scope."
  :group 'extended-faces) ;;; other locals inherit from this by default
(ef-defface sem-hi-scope-broken '((default (:inherit (warning))))
  "A variable accessed beyond its scope.
This is for like JS, accessing a var in a situation where “intuitive” scope
should have ended."
  :group 'extended-faces)
(ef-defface sem-hi-bound ()
  "A variable that appears to be bound."
  :group 'extended-faces)
(ef-defface sem-hi-unbound '((default (:inherit (error))))
  "A variable that appears to not be bound.
NB: Your mode should use this face only when it can statically guarantee the
name is not in scope. Otherwise, it should cycle (like sem-hi-scope-global),
using name hashes."
  :group 'extended-faces)

(ef-defface sem-hi-mutable
            '((default (:inherit (font-lock-variable-name-face))))
  "A variable that is mutable (or refers to a mutable value)."
  :group 'extended-faces)
(ef-defface sem-hi-constant '((default (:inherit (font-lock-constant-face))))
  "A constant."
  :group 'extended-faces)

(ef-defface sem-hi-binding
            '((default (:inherit (font-lock-variable-name-face))))
  "A binding site."
  :group 'extended-faces)

(provide 'extended-faces)

;; Local Variables:
;; read-symbol-shorthands: (("ef-" . "extended-faces-"))
;; End:

;;; extended-faces.el ends here
