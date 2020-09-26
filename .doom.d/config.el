;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "Monospace" :size 28)
      doom-variable-pitch-font (font-spec :family "Sans Serif" :weight 'light))

(setq! org-directory "~/org/")

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

;; company interface that works with variable-pitch-mode
(use-package! company-posframe
  :custom
  (company-posframe-show-metadata nil)
  (company-posframe-show-indicator nil))

(use-package! valign
  :init (setq valign-fancy-bar t))

(setq! +biblio-notes-path "~/Documents/arbeiten/notes/"
       +biblio-pdf-library-dir "~/Documents/arbeiten/pdf/"
       +biblio-default-bibliography-files (getenv "BIB")
       org-ref-default-citation-link "autocite")

(map! (:when (featurep! :lang org)
       (:map org-mode-map
        :localleader
        :desc "Insert citation" "l C" #'org-ref-helm-insert-cite-link)))

(after! org
  (load! "orgtweaks.el"))

(add-hook! 'org-mode-hook
           #'doom-disable-line-numbers-h
           #'variable-pitch-mode
           #'visual-line-mode
           #'org-toggle-pretty-entities
           #'org-fragtog-mode
           #'valign-mode
           #'org-bullets-mode
           #'company-posframe-mode)
