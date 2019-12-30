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
  :custom
  (paradox-github-token t)
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
  :ensure t
  :config (load-theme 'gruvbox t))

;; Set bibligraphies
(setq reftex-default-bibliography  '("~/Documents/arbeiten/refer.bib")
      bibtex-completion-bibliography reftex-default-bibliography
      bibtex-completion-library-path "~/Documents/arbeiten/pdf/"
      bibtex-completion-notes-path   "~/Documents/arbeiten/notes/")

;; Keybinding help menu
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
  ;; Basic definer
  (general-create-definer my-define-almost-everywhere
  :states '(normal visual motion emacs)
  :keymaps 'override)

  (my-define-almost-everywhere
  :prefix "SPC"
  "/"	'(counsel-rg 				:which-key "ripgrep")
  "TAB"	'(switch-to-prev-buffer 		:which-key "previous buffer")
  "SPC"	'(counsel-M-x 				:which-key "M-x")
  ;; Kill ring
  "k"	'(counsel-yank-pop			:which-key "show kill ring")
  ;; Simulated keys
  "w"	(general-simulate-key "C-w" 		:which-key "window operations")
  "h"	(general-simulate-key "C-h" 		:which-key "help")
  ;; Buffers
  "b"	'(counsel-switch-buffer 		:which-key "buffers list")
  ;; Package-management
  "p"	'(list-packages				:which-key "package management")
  ;; Org-mode
  "c"	'(org-ref-helm-insert-cite-link		:which-key "insert citation")
  "l"	'(org-insert-link			:which-key "insert/edit link")
  "o"	'(org-open-at-point			:which-key "open link")
  ;; Quit
  "q"	'(:ignore t				:which-key "quit")
  "qz"	'(delete-frame				:which-key "delete frame")
  "qq"	'(kill-emacs 				:which-key "quit"))

  ;; , for mode-specific prefix
  (my-define-almost-everywhere
  "," (general-simulate-key "C-c"))

  ;; Minibuffer movement
  (general-define-key
  :keymaps '(minibuffer-local-map
	     minibuffer-local-ns-map
	     minibuffer-local-completion-map
	     minibuffer-local-must-match-map
	     minibuffer-local-isearch-map
	     ivy-minibuffer-map)
  "M-j" (general-key "C-n")
  "M-k" (general-key "C-p"))
  )

;; Load other files
(load-file (concat (file-name-directory load-file-name)
		   "org.el"))
(load-file (concat (file-name-directory load-file-name)
		   "prog.el"))
(load-file (concat (file-name-directory load-file-name)
		   "company.el"))
(load-file (concat (file-name-directory load-file-name)
		   "evil.el"))
(load-file (concat (file-name-directory load-file-name)
		   "ivy.el"))

;; prescient for better sorting and filtering
(use-package prescient
  :ensure t)
(use-package ivy-prescient
  :ensure t
  :config (ivy-prescient-mode 1))
(use-package company-prescient
  :ensure t
  :config (company-prescient-mode 1))

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
