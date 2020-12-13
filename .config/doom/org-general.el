;;; -*- lexical-binding: t; -*-

;; org-ref setup
(setq! +biblio-notes-path "~/Documents/arbeiten/notes/"
       +biblio-pdf-library-dir "~/Documents/arbeiten/pdf/"
       +biblio-default-bibliography-files (getenv "BIB")
       org-ref-default-citation-link "autocite")

;; org-roam
(setq org-roam-capture-templates
      '(("d" "default" plain #'org-roam-capture--get-point
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n#+html_head: <link rel=\"stylesheet\" href=\"./css/min.css\">\n#+html_head: <a href=\"index.html\"><div class=\"index\"></div></a>\n\n%?\n\n* Backlinks\n:PROPERTIES:\n:UNNUMBERED: t\n:END:\n#+begin_src elisp :results output raw :exports results\n(dolist (link (org-roam--get-backlinks (buffer-file-name)))\n  (princ (format \"[[file:%s][%s]] \" (car link) (org-roam-db--get-title (car link)))))\n#+end_src"
         :unnarrowed t)))

;; org-roam graph
(setq org-roam-graph-exclude-matcher "index.org")
(setq org-roam-graph-extra-config '(("bgcolor" . "none")))
(setf (car org-roam-graph-node-extra-config) '("shape" . "note"))
(add-to-list 'org-roam-graph-node-extra-config '("fontname" . "sans"))

;; org-publish roam project
(setq org-publish-project-alist
      '(("roam-notes"
         :base-directory "~/org/roam/"
         :base-extension "org"
         :publishing-directory "~/org/roam_html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-mathjax-options ((path "mathjax2/MathJax.js?config=TeX-AMS_HTML") (scale "100") (align "center") (font "TeX") (linebreaks "false") (autonumber "AMS") (indent "0em") (multlinewidth "85%") (tagindent ".8em") (tagside "right"))
         :html-self-link-headlines t
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

;; fontification
(setq org-highlight-latex-and-related '(latex script))
(custom-theme-set-faces! nil
  '(org-block :family "mono")
  '(org-code :family "mono")
  '(org-document-info-keyword :family "mono")
  '(org-formula :family "mono")
  '(org-indent :family "mono")
  '(org-meta-line :family "mono")
  '(org-property-value :family "mono")
  '(org-special-keyword :family "mono")
  '(org-table :family "mono")
  '(org-tag :family "mono")
  '(org-verbatim :family "mono"))
