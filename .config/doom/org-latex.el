;;; -*- lexical-binding: t; -*-

(setq org-latex-compiler "xelatex"
      org-latex-pdf-process (list "latexmk -shell-escape -f -%latex %f"))

(nconc org-latex-default-packages-alist '(("mathrm=sym" "unicode-math" t ("xelatex" "lualatex"))))
(nconc org-latex-default-packages-alist '(("" "icomma" t)))

(after! ox-latex
  (add-to-list 'org-latex-default-packages-alist
               '("" "fontspec" t ("xelatex" "lualatex")))

  (dolist (package '(;; better tables
                     ("" "tabulary")
                     ("" "booktabs")
                     ;; better quotes
                     ("" "csquotes")
                     ;; chemistry
                     ("version=4" "mhchem")
                     ;; support #+LANGUAGE (de for german, fr for french etc)
                     ("AUTO" "babel" t ("pdflatex"))
                     ("AUTO" "polyglossia" t ("xelatex" "lualatex"))))
    (add-to-list 'org-latex-packages-alist package))

  ;; use booktabs by default
  (setq org-latex-tables-booktabs t)
  ;; fancy code block syntax highlighting
  (setq org-latex-src-block-backend 'engraved)

  ;; use \textquote{} when smartquotes are enabled
  (dolist (element org-export-smart-quotes-alist)
    (setcdr (nth 1 element) (plist-put (cdr (nth 1 element)) :latex "\\textquote{"))
    (setcdr (nth 2 element) (plist-put (cdr (nth 2 element)) :latex "}")))

  ;; define additional latex classes
  (add-to-list 'org-latex-classes
               '("scrbook" "\\documentclass[11pt]{scrbook}"
                 ;; ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("scrartcl" "\\documentclass[11pt]{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("kaobook" "\\documentclass[11pt]{kaobook}"
                 ;; ("\\part{%s}" . "\\part*{%s}")
                 ("\\setchapterpreamble[u]{\\margintoc}\\chapter{%s}" . "\\setchapterpreamble[u]{\\margintoc}\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; preview latex fragments using dvisvgm
(setq org-preview-latex-default-process 'dvisvgm)
(add-to-list 'org-preview-latex-process-alist
             '(dvisvgm :programs
                       ("xelatex" "dvisvgm")
                       :description "dvi > svg" :message "you need to install the programs: xelatex and dvisvgm." :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
                       (1.7 . 1.5)
                       :latex-compiler
                       ("xelatex -no-pdf -interaction nonstopmode -shell-escape -output-directory %o %f")
                       :image-converter
                       ("dvisvgm %f -n -b min -c %S -o %O")))
; (add-to-list 'org-preview-latex-process-alist
;              '(katex :programs
;                        ("katex")
;                        :description "latex > mathml" :message "you need to install katex." :image-input-type "txt" :image-output-type "html"
;                        :latex-compiler
;                        ("cp %f %O")
;                        :image-converter
;                        ("katex -d -F mathml -i %f -o %O")))
(setq org-format-latex-header
      (concat "\\documentclass[dvisvgm]{article}\n"
              (mapconcat 'identity
                         (cdr (split-string org-format-latex-header "\n"))
                         "\n")))

; (setq org-html-with-latex 'html)
; (setq org-latex-to-html-convert-command "echo '%i' | katex -d -t -F mathml")

;; scale latex previews according to local text scale and dpi
(defun my-apply-scale (BEG END)
  (plist-put org-format-latex-options :scale
             (*
              (* (* (face-attribute 'default :height)
                    (expt text-scale-mode-step text-scale-mode-amount))
                 0.011)
              (/ (string-to-number (cond ((let ((x-resource-class "Xft"))
                                            (x-get-resource "dpi" "")))
                                         (t "96")))
                 96))))
(advice-add 'org--latex-preview-region :before #'my-apply-scale)
