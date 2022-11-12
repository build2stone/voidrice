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

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(map!
 :n "j" 'evil-next-visual-line
 :n "k" 'evil-previous-visual-line)

(use-package corfu
  :config
  (setq corfu-doc-delay 0.1
        corfu-doc-transition 'hide
        corfu-preview-current 'insert
        corfu-on-exact-match 'insert)
  ;; for some reason, this only works when shadowing the function...
  (defun corfu-next (&optional n)
    "Go forward N candidates."
    (interactive "p")
    (if tempel--active
        (tempel-next 1)
      (let ((index (+ corfu--index (or n 1))))
        (corfu--goto
         (cond
          ((not corfu-cycle) index)
          ((= corfu--total 0) -1)
          ((< corfu--preselect 0) (1- (mod (1+ index) (1+ corfu--total))))
          (t (mod index corfu--total)))))))
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)))
(use-package cape
  :init
  (setq dabbrev-ignored-buffer-regexps '("^.*\\.pdf$")))

(use-package tempel
  :bind (([tab] . tempel-next)
         ([backtab] . tempel-previous))
  :init
  (setq tempel-path "~/.config/doom/templates"
        tempel-trigger-prefix ",")
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package selectrum
  :bind (:map selectrum-minibuffer-map ("C-j" . next-line)))

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
  (load! "org-links.el"))

(setq org-pretty-entities t
      org-startup-folded 'showall)
(add-hook! 'org-mode-hook
           #'variable-pitch-mode
           #'visual-line-mode
           #'doom-disable-line-numbers-h)

(run-hooks 'doom-first-input-hook)
