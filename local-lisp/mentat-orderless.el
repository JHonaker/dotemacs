;;; mentat-orderless.el -*- lexical-binding: t -*-

(defgroup mentat-orderless ()
  "Tweaks for the Orderless completion style")

(defcustom mentat-orderless-default-styles
  '(orderless-flex
    orderless-initialism
    orderless-regexp
    orderless-prefixes
    orderless-literal)
  "List that should be assigned to `orderless-matching-styles'."
  :type 'list
  :group 'mentat-orderless)

(defun mentat-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun mentat-orderless-initialism-dispatcher (pattern _index _total)
    "Leading initialism style dispatcher using the comman as a prefix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 0 -1))))

(defun mentat-orderless-flex-dispatcher (pattern _index _total)
    "Flex style dispatcher using the tilde as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

(provide 'mentat-orderless)
