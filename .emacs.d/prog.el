;; prog-mode settings and plugins

;; Smartparens for pairing parentheses
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlight strings representing colors
(use-package rainbow-mode
  :ensure t
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Electric-pair-mode
; (add-hook 'prog-mode-hook 'electric-pair-mode)

;; Auto-indent
(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable))

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

