**NB**: This set of faces is necessarily subjective. It’s not defined in isolation, but with input from various packages and the kinds of content they want to be able to distinguish.

# Extended Faces for Emacs

[https://img.shields.io/endpoint.svg?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Femacs-extended-faces](https://garnix.io)
[https://repology.org/badge/tiny-repos/emacs:extended-faces.svg](https://repology.org/project/emacs:extended-faces/versions)
[https://repology.org/badge/latest-versions/emacs:extended-faces.svg](https://repology.org/project/emacs:extended-faces/versions)

This is an overhaul of Emacs faces designed to give package creators maximum flexibility, while making it easy for theme creators to support many packages without a bunch of custom faces and minimizing and minimizing the need for users to override specific faces.

## some goals of this package:

1. define Emacs built-in faces using _only_ `:inherit` (sometimes this doesn’t make sense – for example, the faces `bold`, `fixed-pitch`, etc. that are named for the specific attributes they embody – but otherwise);
2. handle all other initial styling as a `default` theme (except for the `default` face, which must be fully defined); and
3. introduce new faces into the hierarchy, with the intent of 3rd-party packages being able to define _their_ faces using only `:inherit` and theme authors being able to create a usable theme without needing to redefine hundreds of faces across dozens of packages.

## a few points of complication

The `error`, `warning`, and `success` faces are ambiguous as to whether they should be used to highlight erroneous constructs themselves, or messages referring to errors, etc. The docstring for `error` says “Basic face used to highlight errors and to denote failure.” But there is also `font-lock-warning-face`, which isn’t meant to highlight warning messages, or even things which may be wrong, but rather constructs that can affect the behavior of the language as a whole, and thus should be taken note of.

So, a big part of this project is identifying that intent, and trying to make a hierarchy of faces that respects it.

## usage

### users

You don’t have to do any of this for it to work, but if you do want to change the faces for your system, here is the preferred way to do it given the structure of this package.

1. customize any of the `fixed-pitch`, `fixed-pitch-serif`, and `variable-pitch` faces with explicit `family` (and perhaps `foundry`);
2. customize the `default` face with the desired `height` and set its `family`, most likely to the same family as one of the faces in step 1 (don’t `inherit`); and
3. customize some “high-level” faces, notably `font-lock` (for writing code) and `text`. Most commonly they should simply `inherit` from one of the faces in step 1, but it’s not uncommon to have a programming-specific font (for example, [Fira Code](https://github.com/tonsky/FiraCode) might be used as `font-lock`’s `family` when [Fira Mono](https://mozilla.github.io/Fira/) is used as `fixed-pitch`’s `family`).

If you use a theme built on `extended-faces`, that should be all you need.

If you use a theme _not_ built on `extended-faces` (or no theme at all), then load the `interim-faces` theme _before_ your theme (which means after it in `custom-themes-enabled` (for example, `(setq custom-enabled-themes '(my-favorite-theme interim-faces)`). This will connect the graph, but allow your theme to override faces at any point in the graph, so the packages it explicitly themes will still be themed, but other faces they missed will also be picked up.

### theme authors

The primary goal of this package is to make it easy for theme authors to create a theme without having to customize faces for every package known to MELPA (and beyond).

Start by extending the `interim-faces` theme. (**TODO**: How? Just copy it? Yuck.)

Go light – don’t customize every face you see, but if you come across one you want to customize, look it up in the graph and follow its ancestors (and the descendants of those ancestors to find the highest-level face(s) that you actually want to apply the customization to.

**TODO**: Add some Emacs functions to be able to explore the graph – helm(-like) view to look up through a graph of ancestors, then seeing the descendants of each ancestor (to make it clear what else you’re customizing).

### other package authors

#### guidelines

* don’t use faces from Emacs or other packages directly – define your own version (usually just by prefixing your package name) and then inherit from the upstream face. This gives users the maximum ability to customize with the minimum need to do so.
* in general, try to refer to each of your faces about once in your code, and liberally create “unused” faces simply to collect other sub-faces via inheritance for simplified theming.
* If you’re making a major mode (or a minor mode that draws something like a rectangular region (for example, `popup-mode`), you should create a face named something like `my-mode-default` that inherits from `default` (or can inherit from `variable-pitch`, `fixed-pitch`, `font-lock`, etc. if there is a reason for the buffer to require one of those), which all your other faces that draw in the buffer area can have in their inheritance. This face should also be used for `buffer-face-set`.
* don’t have variables containing face mappings (for example, `ensime-sem-high-faces`), but define distinct faces and use them directly.
* if you feel like there is a particular set of colors/properties that work for your package, define a single variable to disable them [ed: it would be great to come up with a standard name for this variable to make it easy to find] so users can turn them off if they clash with the user’s theme.
* specific font families shouldn’t be mentioned outside of a user’s own settings.

#### General approach for creating faces

1. figure out the minimum set of faces you need to distinguish between important things (a judgement call);
2. for all these faces, define them in terms of `:inherit`, referring to some face included in Emacs or in this package; then
3. think of the maximum set of things you’d like to distinguish and define faces for these via `:inherit` to a face defined in step 1 or a face included in Emacs or in this package.

## Other things planned for this package

### defining unified set of faces for syntactic and semantic highlighting

The distinction between the two varies by language – in some languages it’s easy to identify whether something is a type name syntactically, in others, it requires semantic analysis.

Separating different aspects: binding/use, function/value/type, etc., expecting them to be combined at point of use.

### standardizing parent faces for various purposes

Sets of faces that represent various concepts across different formats. For example, `text-heading` to be inherited by `markdown-header-face` as well as `info-title-*`; generalized levels for outlines and nested headers; urgency levels for alerts, logs, etc.; `filesystem-directory`, `-file`, `-symlink`, etc. for use by dired, speedbar, etc.

## building

### preferred

Especially if you are unfamiliar with the Emacs ecosystem, there is a Nix build (both with and without a flake).

### traditional

This project is built with [Eldev](https://doublep.github.io/eldev/).
