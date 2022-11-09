;;; completion/selectrum/config.el -*- lexical-binding: t; -*-

;; TODO: evaluate raxod502's snippets at https://github.com/raxod502/selectrum/wiki/Additional-Configuration
;; TODO: consult-buffer mapping should be conditional (featurep :ui workspaces ??)

(add-hook! '(doom-first-input-hook)
  (when (modulep! +orderless)
    (require 'orderless))
  (selectrum-mode +1)
  (marginalia-mode +1)
  (when (modulep! +prescient)
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1))
  (require 'embark))

(use-package! selectrum
  :commands (selectrum-mode)
  :init
  (setq selectrum-fix-vertical-window-height t)
  :config
  (map! :leader :desc "Repeat last selectrum" "'" #'selectrum-repeat))

(use-package! selectrum-prescient
  :when (modulep! +prescient)
  :commands (selectrum-prescient-mode))

(defun doom-selectrum--flex-if-twiddle (pattern _index _total)
  (when (string-prefix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 1))))

(defun doom-selectrum--without-if-bang (pattern _index _total)
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

(defun doom-selectrum--prefixes-if-slash (pattern _index _total)
  (when (string-prefix-p "/" pattern)
    `(orderless-prefixes . ,(substring pattern 1))))

(use-package! orderless
  :when (modulep! +orderless)
  :init
  (setq selectrum-refine-candidates-function #'orderless-filter
        selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq orderless-matching-styles '(orderless-regexp)
        orderless-style-dispatchers '(doom-selectrum--flex-if-twiddle
                                      doom-selectrum--without-if-bang
                                      doom-selectrum--prefixes-if-slash))
  :config
  (setq completion-styles '(orderless)))

(use-package! embark
  :bind (("C-S-a" . embark-act))
  :config
  (setq embark-collect-initial-view-alist '((t . zebra)))
  (setq embark-collect-live-update-delay 0.5)
  (setq embark-collect-live-initial-delay 0.8)

  ;; This produces an effect similar to (setq resize-mini-windows t) for the minibuffer.
  (add-hook 'embark-collect-post-revert-hook
            (defun resize-embark-live-occur-window (&rest _)
              (when (string-match-p "Live" (buffer-name))
                (fit-window-to-buffer (get-buffer-window)
                                      (floor (frame-height) 2) 1))))

  (add-hook 'embark-target-finders 'selectrum-get-current-candidate)
  ;; This hook is not useful if there's already the selectrum backend
  ;; (add-hook 'minibuffer-setup-hook 'embark-live-occur-after-input)

  (add-hook 'embark-candidate-collectors
            (defun embark-selectrum-candidates+ ()
              (when selectrum-active-p
                (selectrum-get-current-candidates
                 ;; Pass relative file names for dired.
                 minibuffer-completing-file-name))))

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)

  (add-hook 'embark-input-getters
            (defun embark-selectrum-input-getter+ ()
              (when selectrum-is-active
                (let ((input (selectrum-get-current-input)))
                  (if minibuffer-completing-file-name
                      ;; Only get the input used for matching.
                      (file-name-nondirectory input)
                    input)))))

  ;; The following is not selectrum specific but included here for convenience.
  ;; If you don't want to use which-key as a key prompter skip the following code.

  ;; (setq embark-action-indicator
  ;;       (defun embark-which-key-setup+ ()
  ;;         (let ((help-char nil)
  ;;               (which-key-show-transient-maps t)
  ;;               (which-key-replacement-alist
  ;;                (cons '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
  ;;                      which-key-replacement-alist)))
  ;;           (setq-local which-key-show-prefix nil)
  ;;           (setq-local which-key-persistent-popup t)
  ;;           (which-key--update)))
  ;;       embark-become-indicator embark-action-indicator)

  ;; (add-hook 'embark-pre-action-hook
  ;;           (defun embark-which-key-tear-down+ ()
  ;;             (kill-local-variable 'which-key-persistent-popup)
  ;;             (kill-local-variable 'which-key-show-prefix)
  ;;             (unless which-key-persistent-popup
  ;;               (which-key--hide-popup-ignore-command))))
  )

;; Projectile defaults to forcing icomplete instead of completing-read
(after! projectile
  (setq projectile-completion-system 'default))

(defun +consult/project-search (&optional arg initial-query directory)
  "Conduct a text search in the current project root."
  (interactive "P")
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (current-prefix-arg (unless (eq arg 'other) arg))
         (default-directory
           (if (eq arg 'other)
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             default-directory)))
    (consult-ripgrep directory initial-query)))

(defun +consult/search-project-for-symbol-at-point (&optional symbol arg)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))
         current-prefix-arg))
  (let* ((projectile-project-root nil)
         (default-directory
           (if arg
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             default-directory)))
    (+consult/project-search nil symbol)))

;; Example configuration for Consult
(use-package! consult
  ;; Replace bindings. Lazily loaded due to use-package.
  :bind (([remap +default/search-project] . +consult/project-search)
         ([remap +default/search-project-for-symbol-at-point] . +consult/search-project-for-symbol-at-point)
         ("C-c h" . consult-history)
         ("C-c o" . consult-outline)
         ;; consult buffer is useless because we're using
         ;; :ui workspaces for now
         ;; ("C-x b" . consult-buffer)
         ;; ("C-x 4 b" . consult-buffer-other-window)
         ;; ("C-x 5 b" . consult-buffer-other-frame)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ([remap bookmark-jump] . consult-bookmark)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g l" . consult-line)
         ("M-g i" . consult-imenu)
         ("M-g e" . consult-error)
         ([remap load-theme] . consult-theme)
         ([remap multi-occur] . consult-multi-occur)
         ([remap yank-pop] . consult-yank-pop)
         ([remap apropos] . consult-apropos)
         ([remap recentf-open-files] . consult-recent-file))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  (dolist (binding
           '((imenu . consult-imenu)
             (bookmark-jump . consult-bookmark)))
    (map! :g (vector 'remap (car binding)) (cdr binding)))

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package
  :config
  (setq consult-project-root-function #'doom-project-root
        consult-preview-key (list (kbd "<C-right>") (kbd "<C-left>"))))

(use-package! consult-selectrum
  :after (consult selectrum))

(use-package! marginalia
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

;; Install the consult-flycheck command.
(use-package! consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))
