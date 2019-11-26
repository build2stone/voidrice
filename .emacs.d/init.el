;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Makes garbage-collection smooth
(use-package gcmh
  :ensure t
  :init (gcmh-mode 1))

(use-package paradox
  :ensure t
  :config (setq paradox-execute-asynchronously t)
  :init (paradox-enable))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      bidi-paragraph-direction 'left-to-right)

(fset 'yes-or-no-p 'y-or-n-p) 				;; Fast y/n
(setq custom-file (make-temp-file "emacs-custom")) 	;; Add customized settings to temp file
(global-auto-revert-mode t) 				;; Auto-reload files

;; UI configurations
(setq inhibit-startup-screen t
      initial-scratch-message ";; Happy Hacking")
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(blink-cursor-mode -1)

;; Make scrolling nicer
(setq scroll-margin 7
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode  1)

;; Load theme
(use-package gruvbox-theme
  :ensure t)

;; Load other files
(load-file (concat (file-name-directory load-file-name)
		   "org.el"))
(load-file (concat (file-name-directory load-file-name)
		   "prog.el"))
(load-file (concat (file-name-directory load-file-name)
		   "company.el"))
(load-file (concat (file-name-directory load-file-name)
		   "evil.el"))

;; Markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
	helm-mode-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-locate-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	helm-candidate-number-list 80
	helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t
	helm-echo-input-in-header-line t
	helm-autoresize-max-height 0
	helm-autoresize-min-height 20
	helm-buffer-skip-remote-checking t)
  :config
  (helm-mode 1))

;; RipGrep
(use-package helm-rg :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

;; Helm Projectile
(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :config
  (helm-projectile-on))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Custom keybinding
(use-package general
  :ensure t
  :init (general-evil-setup)
  :config
  (general-define-key
  :states '(normal visual motion insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "/"   '(helm-projectile-rg 	:which-key "ripgrep")
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "SPC" '(helm-M-x 		:which-key "M-x")
  ;; Projectile
  "p"	'(:ignore t 				:which-key "projectile")
  "pf"  '(helm-projectile-find-file		:which-key "find files")
  "pp"  '(helm-projectile-switch-project	:which-key "switch project")
  "pb"  '(helm-projectile-switch-to-buffer	:which-key "switch buffer")
  ;; Kill ring
  "k"  '(helm-show-kill-ring	:which-key "show kill ring")
  ;; Buffers
  "b"  '(helm-mini 		:which-key "buffers list")
  ;; Quit
  "q"	'(:ignore t		:which-key "quit")
  "qz"  '(delete-frame		:which-key "delete frame")
  "qq"  '(kill-emacs 		:which-key "quit"))

  ;; Access evil window keybinds with SPC-w
  (general-nmap "<SPC>w" (general-simulate-key "C-w" :which-key "window operations"))
  )

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Statusline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-on)
  (spaceline-toggle-evil-state-on))
