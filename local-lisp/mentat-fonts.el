;;; mentat-fonts.el -*- lexical-binding: t -*-

(defcustom mentat-fonts-typeface-sets-alist
  '((main . (:fixed-pitch-family 
	     :fixed-pitch-regular-weight normal
	     :fixed-pitch-heavy-weight bold
	     :fixed-pitch-height 100
	     :fixed-pitch-line-spacing 1
	     :variable-pitch-family
	     :variable-pitch-height 1.0
	     :variable-pitch-regular-weight normal))))

(provide 'mentat-fonts)
