;; ivy/counsel config
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))
(use-package counsel
  :ensure t)

(setq ivy-on-del-error-function nil		;; backspace on empty line does nothing (as opposed to exiting ivy)
      ivy-height 20
      ivy-count-format 	"(%d/%d) "
      ivy-use-virtual-buffers t)

;; fancy icons
(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))
