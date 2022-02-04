(deftheme my-base
  "The basis for my custom theme.")

(let ((class '((class color) (min-colors 89)))
      (main-bg "#ffffff")
      (main-fg "#000000")
      (red "#a00000")
      (green "#005000")
      (blue "#000077"))
  (custom-theme-set-faces
   'my-base
   `(default ((,class :background ,main-bg :foreground ,main-fg)))
   `(cursor ((,class :background ,red)))
   `(font-lock-builtin-face ((,class :foreground ,blue)))
   `(font-lock-string-face ((,class  :foreground ,green)))))

(provide-theme 'my-base)

(provide 'my-base-theme)
