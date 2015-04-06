**NB**: This set of faces is necessarily very subjective. It is not defined in isolation, but with input from various packages and the kinds of content they want to be able to distinguish.

There are a couple goals of this package:

1. define Emacs built-in faces using _only_ `:inherit` (in some cases, this doesn’t make sense – EG, the faces `bold`, `fixed-pitch`, etc. that are named for the specific attributes they embody – but otherwise);
2. handle all other initial styling as a `default` theme (with the exception of the `default` face, which must be fully defined); and
3. introduce new faces into the hierarchy, with the intent of 3rd-party packages being able to define _their_ faces using only `:inherit` and theme authors being able to create a usable theme without needing to redefine hundreds of faces across dozens of packages.

A few points of complication:

The `error`, `warning`, and `success` faces are ambiguous as to whether they should be used to highlight erroneous constructs themselves, or messages referring to errors, etc. The docstring for `error` says “Basic face used to highlight errors and to denote failure.” But there is also `font-lock-warning-face`, which is not meant to highlight warning messages, or even things which may be incorrect, but rather constructs that can affect the behavior of the language as a whole, and thus should be taken note of.

So, a big part of this project is identifying that intent, and trying to make a hierarchy of faces that respects it.
