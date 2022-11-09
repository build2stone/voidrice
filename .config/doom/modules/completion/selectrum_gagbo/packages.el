;; -*- no-byte-compile: t; -*-
;;; completion/selectrum/packages.el

(package! selectrum)
(package! consult)
(when (modulep! :checkers syntax)
  (package! consult-flycheck))
(package! marginalia)
(package! embark)
(package! ripgrep)
(when (modulep! +prescient)
  (package! selectrum-prescient))
(when (modulep! +orderless)
  (package! orderless))
