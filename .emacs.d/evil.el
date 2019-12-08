;; Evil-mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :init (setq evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package evil-escape
  :ensure t
  :init (setq
	    evil-escape-key-sequence "jk"
	    evil-escape-unordered-key-sequence t)
  :config (evil-escape-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-goggles
  :ensure t
  :init (setq evil-goggles-blocking-duration 0.100)
  :config
  (evil-goggles-use-diff-refine-faces)
  (evil-goggles-mode))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

