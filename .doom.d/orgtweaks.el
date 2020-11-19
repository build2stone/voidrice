;;; -*- lexical-binding: t; -*-

;; use xelatex by default
(setq org-latex-compiler "xelatex"
      org-latex-pdf-process (list "latexmk -shell-escape -f -%latex %f"))

(nconc org-latex-default-packages-alist '(("mathrm=sym" "unicode-math" t ("xelatex" "lualatex"))))
(nconc org-latex-default-packages-alist '(("" "icomma" t)))

(after! ox-latex
  (add-to-list 'org-latex-default-packages-alist
               '("" "fontspec" t ("xelatex" "lualatex")))

  ;; tables
  (add-to-list 'org-latex-packages-alist
               '("" "tabulary"))
  (add-to-list 'org-latex-packages-alist
               '("" "booktabs"))
  ;; quotes
  (add-to-list 'org-latex-packages-alist
               '("" "csquotes"))

  ;; use \textquote{} when smartquotes are enabled
  (dolist (element org-export-smart-quotes-alist)
    (setcdr (nth 1 element) (plist-put (cdr (nth 1 element)) :latex "\\textquote{"))
    (setcdr (nth 2 element) (plist-put (cdr (nth 2 element)) :latex "}")))

  ;; use #+LANGUAGE to set document language (de for german, fr for french etc)
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

  ;; highlight code listings
  (add-to-list 'org-latex-packages-alist
               '("" "minted" nil))
  (setq org-latex-listings 'minted)

  ;; define additional latex classes
  (add-to-list 'org-latex-classes
               '("caesar_book" "\\documentclass[11pt]{caesar_book}"
                 ;; ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
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

;; org-roam
(setq org-roam-capture-templates
      '(("d" "default" plain #'org-roam-capture--get-point
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n#+html_head: <link rel=\"stylesheet\" href=\"./css/min.css\">\n#+html_head: <a href=\"index.html\"><div class=\"index\"></div></a>\n\n%?\n\n* Backlinks\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n#+begin_src elisp :results output raw :exports results\n(dolist (link (org-roam--get-backlinks (buffer-file-name)))\n  (princ (format \"[[file:%s][%s]] \" (car link) (org-roam--get-title-or-slug (car link)))))\n#+end_src"
         :unnarrowed t)))

;; ignore index file in org-roam graph
(setq org-roam-graph-exclude-matcher "index.org")
;; node formatting
(setq org-roam-graph-extra-config '(("bgcolor" . "none")))
(setf (car org-roam-graph-node-extra-config) '("shape" . "note"))
(add-to-list 'org-roam-graph-node-extra-config '("fontname" . "sans"))

(setq org-publish-project-alist
      '(("roam-notes"
         :base-directory "~/org/roam/"
         :base-extension "org"
         :publishing-directory "~/org/roam_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
		 :html-mathjax-options ((path "mathjax2/MathJax.js?config=TeX-AMS_HTML") (scale "100") (align "center") (font "TeX") (linebreaks "false") (autonumber "AMS") (indent "0em") (multlinewidth "85%") (tagindent ".8em") (tagside "right"))
         :headline-levels 4
         :auto-preamble t
         :preparation-function (lambda (args)
                                 (org-roam-db-build-cache)
                                 (f-touch (concat org-roam-directory "index.org"))))
        ("roam-static"
         :base-directory "~/org/roam"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
         :publishing-directory "~/org/roam_html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("org" :components ("roam-notes" "roam-static"))))

;; svg latex previews
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
(setq org-format-latex-header
      (concat "\\documentclass[dvisvgm]{article}\n"
              (mapconcat 'identity
                         (cdr (split-string org-format-latex-header "\n"))
                         "\n")))

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

;; fontification
(setq org-highlight-latex-and-related '(latex script))
(custom-theme-set-faces! nil
  '(org-block :family "mono")
  '(org-code :family "mono")
  '(org-document-info-keyword :family "mono")
  '(org-indent :family "mono")
  '(org-meta-line :family "mono")
  '(org-property-value :family "mono")
  '(org-special-keyword :family "mono")
  '(org-table :family "mono")
  '(org-tag :family "mono")
  '(org-verbatim :family "mono"))
