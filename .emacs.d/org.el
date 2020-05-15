;; org-mode settings and plugins

(use-package org
			 :ensure org-plus-contrib
			 :init (setq org-startup-indented t
						 org-directory "~/org"
						 org-default-notes-file (concat org-directory "/notes.org")
						 org-src-fontify-natively t
						 org-src-tab-acts-natively t
						 org-pretty-entities	t
						 org-highlight-latex-and-related '(latex script)
						 org-list-indent-offset 1)
			 )

;; html-export
(use-package htmlize
			 :ensure t)

;; Set up Symbola as symbol font
(setq org-latex-packages-alist '(
"
\\usepackage{fontspec}
\\setmainfont{Latin Modern Roman}
\\newfontfamily{\\defaultfont}{Latin Modern Roman}
\\newfontfamily{\\symbolfont}{Symbola}
\\usepackage[Latin,Mathematics,NumberForms,Punctuation,Symbols]{ucharclasses}
\\setTransitionsForSymbols{\\symbolfont}{\\defaultfont}
\\setTransitionsFor{NumberForms}{\\symbolfont}{\\defaultfont}
\\setTransitionsForMathematics{\\symbolfont}{\\defaultfont}
\\setTransitionTo{Punctuation}{\\defaultfont}
\\setTransitionTo{Latin}{\\defaultfont}"))

;; Paragraph formatting
(add-to-list 'org-latex-packages-alist
			 '("" "parskip"))
;; Paper size, margins
(add-to-list 'org-latex-packages-alist
			 '("a4paper, margin=2cm, truedimen" "geometry"))
;; Better tables
(add-to-list 'org-latex-packages-alist
			 '("" "tabulary"))
;; Quotes
(add-to-list 'org-latex-packages-alist
			 '("" "csquotes"))

;; Use \textquote{} when smartquotes are enabled
(with-eval-after-load "ox"
    (dolist (element org-export-smart-quotes-alist)
	  (setcdr (nth 1 element) (plist-put (cdr (nth 1 element)) :latex "\\textquote{"))
	  (setcdr (nth 2 element) (plist-put (cdr (nth 2 element)) :latex "}"))))

;; Use #+LANGUAGE to set document language (de for german, fr for french etc)
(add-to-list 'org-latex-packages-alist
			 '("AUTO" "babel" t ("pdflatex")))
(add-to-list 'org-latex-packages-alist
			 '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

; ;; Hijack language key definitions to use ziffer (germany-appropriate maths punctuation) when document language is german
(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-polyglossia-language-alist
		(list "de" "german}\n\\usepackage{ziffer" "german")))

;; Use xelatex
(setq org-latex-compiler "xelatex"
	  org-latex-pdf-process (list "latexmk -shell-escape -f -pdfxe %f"))

;; Caption below
(setq org-latex-caption-above nil)

;; For better references
(use-package org-ref
			 :ensure t
			 :init
			 (setq org-ref-default-bibliography				reftex-default-bibliography
				   org-ref-pdf-directory					bibtex-completion-library-path
				   org-ref-bibliography-notes				bibtex-completion-notes-path
				   bibtex-completion-pdf-open-function		'org-open-file
				   org-ref-default-citation-link			"autocite"
				   org-latex-prefer-user-labels t)) ;; That last one is required to make ref:NAME links work

;; From here on out everything is cosmetic (not affecting the final document)

;; org-tempo for quick structure templates (ex.: <s<TAB> for source block) as described here https://orgmode.org/manual/Structure-Templates.html
(require 'org-tempo)

;; Latex previews

;; Use dvisvgm and xelatex
(setq org-latex-create-formula-image-program 'dvisvgm)
(add-to-list 'org-preview-latex-process-alist
	     '(dvisvgm :programs
		       ("xelatex" "dvisvgm")
		       :description "dvi > svg" :message "you need to install the programs: xelatex and dvisvgm." :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
		       (1.7 . 1.5)
		       :latex-compiler
		       ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
		       :image-converter
		       ("dvisvgm %f -n -b min -c %S -o %O")))

;; Move latex preview files to ~/.cache
(setq org-preview-latex-image-directory "~/.cache/emacs/ltximg/")

;; Toggle previews when cursor enters
(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; Scale latex previews according to default font-face height and correct for x dpi setting
(defun my-apply-scale ()
  (plist-put org-format-latex-options :scale (*
											   (* (face-attribute 'default :height) 0.011)
											   (/ (string-to-number (cond ((let ((x-resource-class "Xft"))
																			 (x-get-resource "dpi" "")))
																		  (t "96")))
												  96)
											   )
			 ))

(defun my-latex-preview-hook ()
  (my-apply-scale)
  (org--latex-preview-region (point-min) (point-max)))
(add-hook 'org-mode-hook 'my-latex-preview-hook)

;; Change text-scaling binds defined in init.el to update latex preview scale
;; Reload with C-u C-u C-c C-x C-l
(defun my-text-scale-increase ()
  (interactive)
  (default-text-scale-increase)
  (my-apply-scale))

(defun my-text-scale-decrease ()
  (interactive)
  (default-text-scale-decrease)
  (my-apply-scale))

(defun my-text-scale-reset ()
  (interactive)
  (default-text-scale-reset)
  (my-apply-scale))

(general-define-key
    :keymaps 'default-text-scale-mode-map
    "M-C-S-k" 'my-text-scale-increase
    "M-C-S-j" 'my-text-scale-decrease
    "M-C-)" 'my-text-scale-reset)

;; Faces - Text font, colour and size
(let* ((font 			'(:family "Sans Serif"))
	   (headline		`(:inherit default :weight bold)))

  (custom-theme-set-faces
	'user
	'(variable-pitch ((t (:family "Sans Serif" :weight light))))
	'(fixed-pitch ((t (:family "Mono" :slant normal :weight normal :width normal))))

	'(org-block ((t (:inherit fixed-pitch))))
	'(org-code ((t (:inherit (shadow fixed-pitch)))))
	'(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
	'(org-indent ((t (:inherit (org-hide fixed-pitch)))))
	'(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
	'(org-property-value ((t (:inherit fixed-pitch))) t)
	'(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
	'(org-table ((t (:inherit fixed-pitch))))
	'(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
	'(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(set-face-attribute 'org-document-title nil :height 2.0)
(set-face-attribute 'org-level-1 nil :height 1.75)
(set-face-attribute 'org-level-2 nil :height 1.5)
(set-face-attribute 'org-level-3 nil :height 1.25)
(set-face-attribute 'org-level-4 nil :height 1.1)

;; Enable variable text pitch
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Enable visual line mode in org-mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; Fancy unicode bullets
(use-package org-bullets
			 :ensure t
			 :init (add-hook 'org-mode-hook 'org-bullets-mode))

;; Replace list dashes with unicode dots
(font-lock-add-keywords 'org-mode
						'(("^ *\\([-]\\) "
						   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
