(defun jh/find-user-init-file (&optional open-current-window-p)
  "Edit the `user-init-file', in another window. C-u to open in current window."
  (interactive "P")
  (if open-current-window-p
      (find-file user-init-file)
    (find-file-other-window user-init-file)))
;; (global-set-key (kbd "C-c e i") 'jh/find-user-init-file)
