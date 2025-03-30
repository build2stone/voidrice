;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "mono")
      doom-symbol-font (font-spec :family "IPAPMincho")
      ;; doom-variable-pitch-font (font-spec :family "Fira Sans" :weight 'light)
      )

(set-face-attribute 'variable-pitch nil :weight 'light)

(setq display-line-numbers-type 'relative
      scroll-margin 7
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      jit-lock-defer-time 0)

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(map!
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line)

(use-package corfu
  :config
  (setq corfu-popupinfo-delay 0.1
        corfu-preview-current 'insert
        corfu-preselect 'prompt)
  ;; for some reason, this only works when shadowing the function...
  (defun corfu-next (&optional n)
    "Go forward N candidates."
    (interactive "p")
    (if (bound-and-true-p tempel--active)
        (tempel-next 1)
      (let ((index (+ corfu--index (or n 1))))
        (corfu--goto
         (cond
          ((not corfu-cycle) index)
          ((= corfu--total 0) -1)
          ((< corfu--preselect 0) (1- (mod (1+ index) (1+ corfu--total))))
          (t (mod index corfu--total)))))))
  :bind (:map corfu-map
              ([tab] . corfu-next)
              ([backtab] . corfu-previous)))
(use-package cape
  :init
  (setq dabbrev-ignored-buffer-regexps '("^.*\\.\\(pdf\\|png\\|jpeg\\|jpg\\)$")))

(defun expand-or-space ()
  (interactive)
  (let ((completion-at-point-functions (list 'tempel-expand)))
    (unless (completion-at-point)
      (insert-char ?\s))))
(map! :i "SPC" 'expand-or-space)

(use-package tempel
  :bind (:map tempel-map
              ([tab] . tempel-next)
              ([backtab] . tempel-previous))
  :init
  (setq tempel-path "~/.config/doom/templates"
        tempel-trigger-prefix ",")
  :config
  ;; tempel helper functions
  (defun tempel-org-latex-maybe-wrap (elt)
    (when (eq (car-safe elt) 'maybe-wrap-latex)
      (let ((template (cdr elt)))
        (if (org-inside-LaTeX-fragment-p)
            `(l ,@template)
          `(l "$" ,@template "$")))))
  (add-to-list 'tempel-user-elements #'tempel-org-latex-maybe-wrap))

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

(use-package! org-timeline
  :hook (org-agenda-finalize . org-timeline-insert-timeline))

(use-package! org-modern
  :config
  (setq org-modern-table nil)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package! engrave-faces
  :after ox)

(use-package ox-epub
  :after ox)

(setq! org-directory (file-truename "~/Documents/org"))

(after! org
  (load! "org-general.el")
  (load! "org-latex.el")
  (load! "org-links.el")
  (load! "org-roam-publish.el"))

(setq! citar-bibliography '("~/Documents/bib.bib")
       citar-library-paths '("~/Documents/papers/"))

(setq org-pretty-entities t
      org-startup-folded 'showall
      org-pretty-entities-include-sub-superscripts nil)
(add-hook! 'org-mode-hook
           #'variable-pitch-mode
           #'visual-line-mode
           #'doom-disable-line-numbers-h)

(use-package! typst-mode)
(add-hook! 'typst-mode-hook #'lsp)
(after! lsp-mode
  ;; (define-derived-mode typst-mode prog-mode "typst"
  ;;   "Major mode for typst files.")
  ;; (add-to-list 'lsp-language-id-configuration '(typst--markup-mode . "typst"))
  (add-to-list 'lsp-language-id-configuration '("\\.typ$" . "typst"))
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection "typst-lsp")
                        :activation-fn (lsp-activate-on "typst")
                        :server-id 'typst-lsp)))
(custom-theme-set-faces! nil
  '(org-verbatim :family "mono")
  '(typst-mode-markup-slash-face :foreground "#fabd2f")
  '(typst-mode-markup-label-reference-face :foreground "#fabd2f"))

(run-hooks 'doom-first-input-hook)
