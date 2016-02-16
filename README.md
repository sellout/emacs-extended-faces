**NB**: This set of faces is necessarily very subjective. It is not defined in isolation, but with input from various packages and the kinds of content they want to be able to distinguish.

# Extended Faces for Emacs

This is an overhaul of Emacs faces designed to give package creators maximum flexibility, while making it easy for theme creators to support many packages without a bunch of custom faces and minimizing and minimizing the need for users to override specific faces.

## some goals of this package:

1. define Emacs built-in faces using _only_ `:inherit` (in some cases, this doesn’t make sense – EG, the faces `bold`, `fixed-pitch`, etc. that are named for the specific attributes they embody – but otherwise);
2. handle all other initial styling as a `default` theme (with the exception of the `default` face, which must be fully defined); and
3. introduce new faces into the hierarchy, with the intent of 3rd-party packages being able to define _their_ faces using only `:inherit` and theme authors being able to create a usable theme without needing to redefine hundreds of faces across dozens of packages.

## A few points of complication

The `error`, `warning`, and `success` faces are ambiguous as to whether they should be used to highlight erroneous constructs themselves, or messages referring to errors, etc. The docstring for `error` says “Basic face used to highlight errors and to denote failure.” But there is also `font-lock-warning-face`, which is not meant to highlight warning messages, or even things which may be incorrect, but rather constructs that can affect the behavior of the language as a whole, and thus should be taken note of.

So, a big part of this project is identifying that intent, and trying to make a hierarchy of faces that respects it.

## Guidelines for package creators

* don’t use faces from Emacs or other packages directly – define your own version (usually just by prefixing your package name) and then inherit from the upstream face. This gives users the maximum ability to customize with the minimum need to do so.
* in general, try to refer to each of your faces approximately once in your code, and liberally create “unused” faces simply for inheritance.
* If you’re making a major mode (or a minor mode that draws something like a rectangular region (e.g., popup-mode), you should create a face named something like `my-mode-default` that inherits from `default` (or can inherit from `variable-pitch`, `fixed-pitch`, `font-lock`, etc. if there is a reason for the buffer to require one of those), which all of your other faces that draw in the buffer area can have in their inheritance. This face should also be used for `buffer-face-set`.
* don’t have variables containing face mappings (EG, `ensime-sem-high-faces`), but define distinct faces and use them directly.
* if you feel like there is a particular set of colors/properties that work for your package, provide a single variable to disable them [ed: it would be great to come up with a de facto name for this variable to make it easy to find] so users can easily turn them off if they clash with the user’s theme

## General approach for creating faces

1. figure out the minimum set of faces you need to distinguish between important things (obv. a judgement call);
2. for all of these faces, define them in terms of `:inherit`, referring to some face included in emacs or in this package; then
3. think of the maximum set of things you’d like to distinguish and define faces for these via `:inherit` to a face defined in step 1 or a face included in emacs or in this package.

## Other things planned for this package

### defining unified set of faces for syntactic and semantic highlighting

The distinction between the two varies by language – in some languages it’s easy to determine whether something is a type name syntactically, in others, it requires semantic analysis.

Separating different aspects: binding/use, function/value/type, etc., expecting them to be combined at point of use.

### standardizing parent faces for various purposes

Sets of faces that represent various concepts across different formats. EG, `text-heading` to be inherited by `markdown-header-face` as well as `info-title-*`; generalized levels for outlines and nested headers; urgency levels for alerts, logs, etc.; `filesystem-directory`, `-file`, `-symlink`, etc. for use by dired, speedbar, etc.

