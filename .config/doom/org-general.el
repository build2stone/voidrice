;;; -*- lexical-binding: t; -*-

(after! ob-ditaa
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar"))

;; org-ref setup
(setq! +biblio-notes-path "~/Documents/arbeiten/notes/"
       +biblio-pdf-library-dir "~/Documents/arbeiten/pdf/"
       +biblio-default-bibliography-files (getenv "BIB")
       org-ref-default-citation-link "autocite")

;; org-roam
(setq org-roam-capture-templates
      '(("d" "default" plain "%[~/.config/doom/template.org]"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("l" "default+latex" plain "%[~/.config/doom/template-latex.org]"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))


;; org-roam graph
(setq org-roam-graph-extra-config '(("bgcolor" . "none")
                                    ("overlap" . "false")
                                    ("outputorder" . "edgesfirst")
                                    ("splines" . "true")
                                    ("sep" . "\"+5\"")))
(setq org-roam-graph-executable "neato")

(defun my-synchronous-org-roam-graph-svg (NODE DISTANCE)
  "Returns an svg-format graph of the surroundings of NODE, up to DISTANCE (DISTANCE 0 graphs everything)
Replaces org-protocol links with relative links to exported html files."
  (require 'org-roam-graph)
  (require 'svg)
  (let* ((org-roam-graph-link-hidden-types '("file" "http" "https"))
         (graph (org-roam-graph--dot (org-roam-graph--connected-component
                                      (org-roam-node-id NODE) DISTANCE)))
         (temp-dot (make-temp-file "graph." nil ".dot" graph)))

    (with-temp-buffer
      (insert (shell-command-to-string (concat org-roam-graph-executable " " temp-dot " " "-Tsvg")))

      (let
          ((svggraph (libxml-parse-xml-region (point-min) (point-max))))

        (dolist (tag (dom-by-tag svggraph 'a))
          (dom-set-attribute tag 'href
                             (url-hexify-string (concat
                                                 (file-name-base
                                                  (org-roam-node-file
                                                   (org-roam-node-from-id
                                                    (substring (url-unhex-string (dom-attr tag 'href)) 30))))
                                                 ".html"))))

        (let ((svgtag (dom-by-tag svggraph 'svg)))
          (dom-set-attribute svgtag 'width "100%")
          (dom-set-attribute svgtag 'height "auto")
          (with-temp-buffer
            (xml-print svgtag)
            (buffer-string)))))))

(defun my-synchronous-org-roam-graph-pdf (NODE DISTANCE)
  "Returns a pdf-format graph of the surroundings of NODE, up to DISTANCE (DISTANCE 0 graphs everything)
TODO: convert links similarly to svg version"
  (require 'org-roam-graph)
  (let* ((org-roam-graph-link-hidden-types '("file" "http" "https"))
         (graph (org-roam-graph--dot (org-roam-graph--connected-component
                                      (org-roam-node-id NODE) DISTANCE)))
         (temp-dot (make-temp-file "graph." nil ".dot" graph)))
    (shell-command-to-string (concat org-roam-graph-executable " " temp-dot " " "-Tpdf"))))

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
         :section-numbers 2
         :auto-preamble t
         :preparation-function (lambda (args)
                                 (org-roam-db-sync)
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
  '(org-superstar-header-bullet :family "mono")
  '(org-table :family "mono")
  '(org-tag :family "mono")
  '(org-verbatim :family "mono"))
