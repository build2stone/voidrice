;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "Monospace" :size 26)
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

(setq org-pretty-entities t)
(add-hook! 'org-mode-hook
           #'variable-pitch-mode
           #'visual-line-mode
           #'doom-disable-line-numbers-h)
