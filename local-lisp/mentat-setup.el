(defvar mentat-emacs-init-org "main-init.org"
  "Name of the Org mode file that provides the reference
implementation of the init file.")

(defvar mentat-emacs-init-el "tangled-main-init.el"
  "Name of the tangled init file from init.org")

(defun mentat-emacs-load-config ()
  "Load the main configuration.

First look for `mentat-emacs-init-el' and if that doesn't exist,
look for and load `mentat-emacs-init-org'."
  (let ((init-el-file (locate-user-emacs-file mentat-emacs-init-el))
	(init-org-file (locate-user-emacs-file mentat-emacs-init-org)))
    (if (file-exists-p init-el-file)
	(load-file init-el-file)
      (require 'org)
      (org-babel-load-file init-org-file))))

(mentat-emacs-load-config)

;; This is mostly taken from Prot:

;; This setup rebuilds the config from the Org file. Basically this
;; config treats the Org mode file as the source of truth. When the
;; session is closed, Emacs builds and compiles our init files from
;; `mentat-emacs-init-org'.

;; The idea of this is to spend time on close, when we're done
;; compiling and saving changes instead of when we start and want to
;; do something.

;; The following is for when we close the Emacs session.
(declare-function org-babel-tangle-file "ob-tangle")

(defun mentat-emacs-build-config ()
  "Produce Elisp init from the Org dotemacs.
Add this to `kill-emacs-hook' to use the newest file inthe next session."
  (let ((init-el (locate-user-emacs-file mentat-emacs-init-el))
	(init-org (locate-user-emacs-file mentat-emacs-init-org)))
    (when (file-exists-p init-el)
      (delete-file init-el))
    (require 'org)
    (when (file-exists-p init-org)
      (org-babel-tangle-file init-org init-el)
      (byte-compile-file init-el))))

(add-hook 'kill-emacs-hook #'mentat-emacs-build-config)


