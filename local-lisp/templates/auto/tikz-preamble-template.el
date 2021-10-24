(TeX-add-style-hook
 "tikz-preamble-template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8")))
   (TeX-run-style-hooks
    "latex2e"
    "defs"
    "defs_tikz"
    "minimal"
    "minimal10"
    "graphicx"
    "inputenc"
    "tikz"
    "pgfplots"
    "ifthen"
    "pgfmath"))
 :latex)

