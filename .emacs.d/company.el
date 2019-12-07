;; Company mode
(use-package company
:ensure t
:init
(setq company-minimum-prefix-length 3)
(setq company-auto-complete nil)
(setq company-idle-delay 0)
(setq company-require-match 'never)
(setq company-frontends
  '(company-pseudo-tooltip-unless-just-one-frontend
    company-preview-frontend
    company-echo-metadata-frontend))
(setq tab-always-indent 'complete)
(defvar completion-at-point-functions-saved nil)
:config
(global-company-mode 1)
(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
    	(completion-at-point-functions '(company-complete-common-wrapper)))
	(indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
	(let ((completion-at-point-functions completion-at-point-functions-saved))
	(company-complete-common))))

;; lisp completion
(use-package company-lsp
  :ensure t
  :init
  (push 'company-lsp company-backends))

;; math
(use-package company-math
  :ensure t
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-math-disallow-unicode-symbols-in-faces 'org-latex-and-related)
  (add-to-list 'company-math-allow-latex-symbols-in-faces 'org-latex-and-related))

;; Use posframe for compat with variable pitch text
(use-package company-posframe
  :ensure t
  :init
  (add-hook 'org-mode-hook 'company-posframe-mode))

;; Adds a hook that's run on buffer switch
;; Why do I have to install an extra package for this?
(use-package switch-buffer-functions
  :ensure t)

;; Only enable company-posframe-mode in org-mode
;; (It conflicts with company-quickhelp)
(defun my-use-company-posframe-mode-maybe(prev cur)
    (cond ((derived-mode-p 'org-mode)
	    (company-posframe-mode 1))
	  (t
	    (company-posframe-mode -1))))
(add-hook 'switch-buffer-functions 'my-use-company-posframe-mode-maybe)

;; Display docs in company completions
(use-package company-quickhelp
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-quickhelp-mode))

;; Customize company-preview font face
(set-face-attribute 'company-preview nil :foreground "white" :background "#458588")
