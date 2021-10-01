;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "mono")
      ;; doom-variable-pitch-font (font-spec :family "Fira Sans" :weight 'light)
      )

(set-face-attribute 'variable-pitch nil :weight 'light)

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

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package ox-epub
  :after ox)

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

(run-hooks 'doom-first-input-hook)
