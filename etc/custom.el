(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-electric-math nil)
 '(TeX-engine 'luatex)
 '(TeX-engine-alist '((optex "OpTeX" "optex" nil nil)))
 '(TeX-parse-self t)
 '(TeX-source-correlate-start-server nil)
 '(TeX-view-program-list
   '(("Sioyek"
      ("sioyek %o --reuse-instance"
       (mode-io-correlate " --forward-search-file %b --forward-search-line %n --inverse-search \"emacsclient --no-wait +%2:%3 %1\""))
      "sioyek")))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Sioyek")
     (output-html "xdg-open")))
 '(avy-background t)
 '(avy-style 'at-full)
 '(avy-timeout-seconds 0.4)
 '(backup-by-copying t)
 '(custom-safe-themes
   '("4e7672ce1015731d9d6955652f8f1b438420f109d15f662a014fa4e992429b9a" "0657bbbdf081bafc0b56d4d40cb333829eb576c8c519c8028f29afbf9559eb35" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "e5b45bdad913bcaca125b5641a26e2fc9bed6555e5c01e6783c32052a3afc769" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1" "1d89fcf0105dd8778e007239c481643cc5a695f2a029c9f30bd62c9d5df6418d" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "e613c2ffe0d6c9463d67f37275566ab3c47bdd70114fc3387738a4eb292ea156" "a3ce1d58ffe4033c87298e3ea63ae1953aa2fddc4c4fc9de82dcb013825bb1bc" "1747a02911dec6d66aee4aacfd6b090b823151abcd73a5c68c93c022bf34a7ab" "7821eb547889eeec5276350e4ea0a55c6a62d02bd72f9c7d8e9e2447b805896c" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "67f0f440afa2e68d9d00219b5a56308761af45832fb60769d2b2fd36e3fead45" default))
 '(deft-default-extension "org" t)
 '(deft-directory "~/org/zk/")
 '(deft-extensions '("org" "md" "markdown" "txt"))
 '(fill-column 80)
 '(hl-todo-highlight-punctuation ":")
 '(hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("BUG" error bold)
     ("XXX" font-lock-constant-face bold)))
 '(kept-new-versions 10)
 '(kept-old-versions 5)
 '(olivetti-body-width 0.85)
 '(olivetti-minimum-body-width 80)
 '(popper-group-function #'popper-group-by-directory)
 '(popper-reference-buffers
   '("\\*Messages\\*" "Output\\*$" "\\*Async Shell Command\\*" help-mode helpful-mode compilation-mode "\\*.*REPL\\*"))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   '((eval progn
           (require 'lisp-mode)
           (defun emacs27-lisp-fill-paragraph
               (&optional justify)
             (interactive "P")
             (or
              (fill-comment-paragraph justify)
              (let
                  ((paragraph-start
                    (concat paragraph-start "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                   (paragraph-separate
                    (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                   (fill-column
                    (if
                        (and
                         (integerp emacs-lisp-docstring-fill-column)
                         (derived-mode-p 'emacs-lisp-mode))
                        emacs-lisp-docstring-fill-column fill-column)))
                (fill-paragraph justify))
              t))
           (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))
     (eval with-eval-after-load 'yasnippet
           (let
               ((guix-yasnippets
                 (expand-file-name "etc/snippets/yas"
                                   (locate-dominating-file default-directory ".dir-locals.el"))))
             (unless
                 (member guix-yasnippets yas-snippet-dirs)
               (add-to-list 'yas-snippet-dirs guix-yasnippets)
               (yas-reload-all))))
     (eval add-to-list 'completion-ignored-extensions ".go")
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
           ((root-dir-unexpanded
             (locate-dominating-file default-directory ".dir-locals.el")))
           (when root-dir-unexpanded
             (let*
                 ((root-dir
                   (expand-file-name root-dir-unexpanded))
                  (root-dir*
                   (directory-file-name root-dir)))
               (unless
                   (boundp 'geiser-guile-load-path)
                 (defvar geiser-guile-load-path 'nil))
               (make-local-variable 'geiser-guile-load-path)
               (require 'cl-lib)
               (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
           (locate-dominating-file default-directory ".dir-locals.el"))))
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :height 120))))
 '(fixed-pitch ((t (:family "Iosevka" :height 120))))
 '(variable-pitch ((t (:family "Iosevka Etoile" :height 120)))))
