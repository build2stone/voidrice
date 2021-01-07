;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "mono")
      doom-variable-pitch-font (font-spec :family "sans" :weight 'light))

(setq display-line-numbers-type 'relative
      scroll-margin 7
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      jit-lock-defer-time 0)

(map!
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line)

;; contains unicode, which crashes some versions of gradle
(setenv "LF_ICONS" nil)

(setq! org-directory "~/org/")

(after! org
  (load! "org-general.el")
  (load! "org-latex.el")
  (load! "org-links.el"))

(setq org-pretty-entities t
      org-startup-folded 'showall)
(add-hook! 'org-mode-hook
           #'variable-pitch-mode
           #'visual-line-mode
           #'doom-disable-line-numbers-h)
