;; org-mode settings and plugins

(use-package org
  :ensure org-plus-contrib
  :init (setq org-startup-indented t
	        org-directory "~/org"
	        org-default-notes-file (concat org-directory "/notes.org")
	        org-src-fontify-natively t
	        org-src-tab-acts-natively t
	        org-pretty-entities	t
		org-highlight-latex-and-related '(latex script))
  )

;; html-export
(use-package htmlize
  :ensure t)

;; Fancy unicode bullets
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

;; Replace list dashes with unicode dots
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; Latex preview
(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))
(setq org-latex-create-formula-image-program 'dvisvgm)

(setq org-latex-packages-alist '(
"
\\usepackage{parskip}
\\usepackage[margin=2cm] {geometry}
\\usepackage{fontspec}
\\setmainfont{DejaVu Serif}
\\newfontfamily{\\defaultfont}{DejaVu Serif}
\\newfontfamily{\\symbolfont}{Symbola}
\\usepackage[Latin,Mathematics,NumberForms,Punctuation,Symbols]{ucharclasses}
\\setTransitionsForSymbols{\\symbolfont}{\\defaultfont}
\\setTransitionsFor{NumberForms}{\\symbolfont}{\\defaultfont}
\\setTransitionsForMathematics{\\symbolfont}{\\defaultfont}
\\setTransitionTo{Punctuation}{\\defaultfont}
\\setTransitionTo{Latin}{\\defaultfont}
"))

(setq org-latex-pdf-process (list "latexmk -f -pdfxe %f"))

(use-package org-ref
  :ensure t
  :init
  (setq org-ref-default-bibliography	reftex-default-bibliography
        org-ref-pdf-directory		bibtex-completion-library-path
        org-ref-bibliography-notes	bibtex-completion-notes-path))

;; Faces - Text font, colour and size
(let* ((font 			'(:family "Sans Serif"))
       (headline		`(:inherit default :weight bold)))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Sans Serif" :height 1.2 :weight light))))
   '(fixed-pitch ((t (:family "Mono" :slant normal :weight normal :height 1.0 :width normal))))
   `(org-level-8 ((t (,@headline ,@font))))
   `(org-level-7 ((t (,@headline ,@font))))
   `(org-level-6 ((t (,@headline ,@font))))
   `(org-level-5 ((t (,@headline ,@font))))
   `(org-level-4 ((t (,@headline ,@font :height 1.1))))
   `(org-level-3 ((t (,@headline ,@font :height 1.25))))
   `(org-level-2 ((t (,@headline ,@font :height 1.5))))
   `(org-level-1 ((t (,@headline ,@font :height 1.75))))
   `(org-document-title ((t (,@headline ,@font :height 2.0 :underline t :foreground "aquamarine"))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

;; Enable variable text pitch
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Enable visual lines and make evil respect them
(add-hook 'org-mode-hook 'visual-line-mode)
(setq evil-respect-visual-line-mode t)
