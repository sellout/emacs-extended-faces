;;; Code:

(defun set-face-inheritance (name spec)
  (face-spec-set name `((default :inherit ,spec)) 'face-defface-spec))

;;; THE MOST PRIMITIVE FACES

;;; default         The default face, whose attributes are all specified.

;;; bold            These have the attributes indicated by their names (e.g.,
;;; italic          bold has a bold :weight attribute), with all other
;;; bold-italic     attributes unspecified (and so given by default).
;;; underline
;;; fixed-pitch
;;; variable-pitch

;;; shadow          For “dimmed out” text.

;;; link            For clickable text buttons that send the user to a different
;;; link-visited    buffer or “location”.

;;; highlight       For stretches of text that should temporarily stand out.

;;; match           For text matching a search command.

(defface message ()
  "Extended face that covers all messages – errors, warnings, and successes."
  :group 'extended-faces)
(set-face-inheritance 'error   'message)
(set-face-inheritance 'warning 'message)
(set-face-inheritance 'success 'message)

;;; FONT LOCK

(defface font-lock-value-face ()
  "For any value-level construct"
  :group 'font-lock)

(defface font-lock-identifier-face '((default :inherit font-lock-value-face))
  "For any value-level construct"
  :group 'font-lock)

(defface font-lock-literal-face '((default :inherit font-lock-value-face))
  "For any value-level construct"
  :group 'font-lock)

(defface font-lock-module-face '((default :inherit font-lock-value-face))
  "For any larger grouping construct – class, package, module, etc."
  :group 'font-lock)

(defface font-lock-operator-face '((default :inherit font-lock-function-name-face))
  ""
  :group 'font-lock)

(set-face-inheritance 'font-lock-builtin-face           'font-lock-function-name-face)
(set-face-inheritance 'font-lock-comment-delimiter-face 'shadow)
(set-face-inheritance 'font-lock-comment-face           'font-lock-doc-face)
(set-face-inheritance 'font-lock-constant-face          'font-lock-value-face)
;; doc
(set-face-inheritance 'font-lock-function-name-face     'font-lock-identifier-face)
;; keyword
;; negation-char
;; preprocessor
;; regexp-grouping-backslash
;; regexp-grouping-construct
(set-face-inheritance 'font-lock-string-face            'font-lock-literal-face)
;; type
(set-face-inheritance 'font-lock-variable-name-face     'font-lock-identifier-face)
(set-face-inheritance 'font-lock-warning                'warning)

;;; OTHER FACES INCLUDED WITH EMACS

;;; NEW FACES

;;; generic

;;; Define 11 levels only because that is the number Gnus goes up to.
(defface level-1 ()
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-2 '((default :inherit level-1))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-3 '((default :inherit level-2))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-4 '((default :inherit level-3))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-5 '((default :inherit level-4))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-6 '((default :inherit level-5))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-7 '((default :inherit level-6))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-8 '((default :inherit level-7))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-9 '((default :inherit level-8))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-10 '((default :inherit level-9))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)
(defface level-11 '((default :inherit level-10))
  "Accessory face for multi-level things, like outlines. Inherits from the
   previous level so you can customize levels 1–n, then n+1 on will match n."
  :group 'extended-faces)

;;; urgency – used for things like alert and log levels
;;; NB: Consider urgency in the context of the whole system. If you have three
;;;     levels in your mode, perhaps choose moderate/normal/low so that truly
;;;     urgent things in the system can be given more prevalence

(defface urgency-urgent ()
  "Highest urgency."
  :group 'extended-faces)
(defface urgency-high '((default :inherit urgency-urgent))
  "Highest urgency."
  :group 'extended-faces)
(defface urgency-moderate '((default :inherit urgency-high))
  "Highest urgency."
  :group 'extended-faces)
(defface urgency-normal '((default :inherit urgency-moderate))
  "Highest urgency."
  :group 'extended-faces)
(defface urgency-low '((default :inherit urgency-normal))
  "Highest urgency."
  :group 'extended-faces)
(defface urgency-trivial '((default :inherit urgency-low))
  "Highest urgency."
  :group 'extended-faces)

(defface prompt ()
  "A face used for any kind of shell-like input prompt."
  :group 'extended-faces)

;;; explicit colors

(defface black '((default :foreground "black"))
  "Things that are explicitly black. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)
(defface red '((default :foreground "red"))
  "Things that are explicitly red. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)
(defface green '((default :foreground "green"))
  "Things that are explicitly green. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)
(defface yellow '((default :foreground "yellow"))
  "Things that are explicitly yellow. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)
(defface blue '((default :foreground "blue"))
  "Things that are explicitly blue. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)
(defface magenta '((default :foreground "magenta"))
  "Things that are explicitly magenta. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)
(defface cyan '((default :foreground "cyan"))
  "Things that are explicitly cyan. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)
(defface white '((default :foreground "white"))
  "Things that are explicitly white. Should be used rarely, but useful for term colors, etc."
  :group 'extended-faces)

;;; filesystem

(defface fs-directory () "Things that represent a directory."
  :group 'extended-faces)
(defface fs-executable '((default :inherit file))
  "Things that represent an executable."
  :group 'extended-faces)
(defface fs-file () "Things that represent a file."
  :group 'extended-faces)
(defface fs-broken-symlink '((default :inherit (warning symlink)))
  "Things that represent a broken symlink."
  :group 'extended-faces)
(defface fs-symlink () "Things that represent a symlink."
  :group 'extended-faces)

;;; text formatting

(defface text ()
  "A face that all text-specific faces inherit from. This makes it easy to use a
   different font for text modes vs programming modes."
  :group 'extended-faces)

(defface text-heading '((default :inherit text))
  "A heading. This is often combined with the `level` faces, and should usually
   follow the level in the inheritance list (EG, '(level-2 text-header))."
  :group 'extended-faces)

(defface text-emphasis '((default :inherit italic))
  ""
  :group 'extended-faces)

(defface text-emphasis-strong '((default :inherit bold))
  ""
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
;;; add a dimension for values vs types? what about values vs functions? those
;;;   are more different in some languages than in others (can use the standard font-lock faces, perhaps, but I think the _intent_ of those is really more var+binding or func+binding, etc. … but maybe not for type?
;;; - in some envs, you can’t use [refl|introsp]ection to see what globals might exist (cf. Lua), so hashing non-local-var names is a reasonable fallback in this case (maybe ignore themes and just pick colors that have enough contrast vs the `sem-hi-global-scope` background color) but probably still want to annotate with global indicator.

(defface sem-hi-scope-global ()
  "NB: Your mode should use this face only when it can statically guarantee the
   name is in scope. Otherwise, it should cycle, using name hashes."
  :group 'extended-faces)
(defface sem-hi-scope-file ()
  ""
  :group 'extended-faces) ;;; do we want to be explicit about file scope??
(defface sem-hi-scope-local ()
  ""
  :group 'extended-faces) ;;; other locals inherit from this by default
(defface sem-hi-scope-broken '((default :inherit warning))
  "this is for like JS, accessing a var in a situation where “intuitive” scope
   should have ended"
  :group 'extended-faces)
(defface sem-hi-unbound '((default :inherit error))
  "A variable that appears to not be bound. NB: Your mode should use this face
   only when it can statically guarantee the name is not in scope. Otherwise, it
   should cycle (like sem-hi-scope-global), using name hashes."
  :group 'extended-faces)

(defface sem-hi-mutable '((default :inherit font-lock-variable-name-face))
  ""
  :group 'extended-faces)
(defface sem-hi-constant '((default :inherit font-lock-constant-face))
  ""
  :group 'extended-faces)

(defface sem-hi-binding '((default :inherit font-lock-variable-name-face))
  ""
  :group 'extended-faces)

(provide 'extended-faces)
