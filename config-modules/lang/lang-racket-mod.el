;;; ob-racket.el

(provide 'lang-racket)

(straight-use-package '(ob-racket
                        :type git
                        :host github
                        :repo "hasu/emacs-ob-racket"))

(with-eval-after-load 'org
  (add-hook 'ob-racket-pre-runtime-library-load-hook
            #'ob-racket-raco-make-runtime-library))

(provide 'lang-racket-mod)
