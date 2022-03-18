;;; lang-markdown-mod.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  John Honaker

;; Author: John Honaker <john@pop-os>
;; Keywords: 

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

;;; Code:

(straight-use-package 'markdown-mode)

(add-to-list 'auto-mode-alist '("/README\\(|:\\.md\\)?\\'" . gfm-mode))

(with-eval-after-load 'markdown-mode
  
  (defun markdown-compile-pandoc (beg end output-buffer)
    "Compiles markdown with the pandoc program, if available.
Returns its exit code."
    (when (executable-find "pandoc")
      (call-process-region beg end "pandoc" nil output-buffer nil
                           "-f" "markdown"
                           "-t" "html"
                           "--mathjax"
                           "--highlight-style=pygments")))
  (setq markdown-enable-math t		; syntax highlighting for latex fragments
	markdown-enable-wiki-links t
	markdown-italic-underscore t
	markdown-asymmetric-header t
	markdown-fontify-code-blocks-natively t
	markdown-gfm-uppercase-checkbox t ; for compat with org-mode
	markdown-gfm-additional-languages '("sh")
	markdown-make-gfm-checkboxes-buttons t

	markdown-command #'markdown-compile-pandoc
	;; This is set to `nil' by default, which causes a wrong-type-arg error
	;; when you use `markdown-open'. These are more sensible defaults.
	markdown-open-command "xdg-open"

	;; A sensible and simple default preamble for markdown exports that
	;; takes after the github asthetic (plus highlightjs syntax coloring).
	markdown-content-type "application/xhtml+xml"
	markdown-css-paths
	'("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
	  "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
	markdown-xhtml-header-content
	(concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
		"<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
		"<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
		"<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))
  )

(provide 'lang-markdown-mod)
;;; lang-markdown-mod.el ends here
