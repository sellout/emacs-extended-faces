;; Can be 'quartz, 'pango, 'fontconfig, or nil
(defvar graph-font-system 'quartz)

(defun name-in-font-system (font-system list)
  (pcase font-system
    ('fontconfig (mapconcat 'identity list ":"))
    ('pango      (mapconcat 'identity list " "))
    ('quartz     (mapconcat 'identity list "-"))
    (_           (car list))))

(defun output-name (sym)
  (insert "\"" (symbol-name sym) "\""))

(defun graph-face-inheritance ()
  (set-buffer (get-buffer-create "*face-graph*"))
  (insert "digraph {\n")
  (insert "graph [rankdir = \"BT\"]\n")
  (insert "node [penwidth = 0, style = filled]\n")
  (mapc (lambda (face)
          (let ((parents (face-attribute face :inherit)))
            (output-name face)
            (insert " [")
            (insert "fontcolor = \"" (face-attribute face :foreground nil 'default) "\"")
            (insert ", fillcolor = \"" (face-attribute face :background nil 'default) "\"")
            (insert ", fontname = \"" (name-in-font-system graph-font-system
                                                           (append (list (face-attribute face :family nil 'default))
                                                                 (let ((w (face-attribute face :weight nil 'default)))
                                                                   (if (eq w 'normal)
                                                                       nil
                                                                     (list (symbol-name w))))
                                                                 (let ((s (face-attribute face :slant nil 'default)))
                                                                   (if (eq s 'normal)
                                                                       nil
                                                                     (list (symbol-name s)))))) "\"")
            (insert ", fontsize = " (number-to-string (/ (face-attribute face :height nil 'default) 10)))
            (insert "]\n")
            (pcase parents
              ((cl-type null)   nil)
              ((cl-type symbol)
               (unless (or  (eq parents 'default) (eq parents 'unspecified))
                 (output-name face) (insert " -> ") (output-name parents) (insert "\n")))
              ((cl-type list)
               (output-name face)
               (insert " -> { ")
               (mapc (lambda (parent) (output-name parent) (insert " "))
                     parents)
               (insert "}\n")))))
        (face-list))
  (insert "}\n"))
