;;; -*- lexical-binding: t; -*-


;; make org find ditaa
(after! ob-ditaa
  (setq org-ditaa-jar-path (executable-find "ditaa")
        org-ditaa-jar-option ""
        org-babel-ditaa-java-cmd ""
        org-babel-default-header-args '((:results . "file")
                                        (:exports . "results"))))

;; add org-roam files to agenda
(add-to-list 'org-agenda-files (concat org-directory "/roam/"))

;; org-roam templates!
(setq org-roam-capture-templates
      '(("d" "default" plain "%[~/.config/doom/template.org]"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("l" "default+latex" plain "%[~/.config/doom/template-latex.org]"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

;; make org-roam graphs prettier
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
  (let* ((org-roam-graph-link-hidden-types '("attachment" "file" "http" "https" "pdf" "mol" "smol")))

    (with-temp-buffer
      (insert (org-roam-graph--dot (org-roam-graph--connected-component
                                      (org-roam-node-id NODE) DISTANCE)))
      (call-process-region (point-min) (point-max) org-roam-graph-executable t '(t nil) nil "-Tsvg")

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
  (let* ((org-roam-graph-link-hidden-types '("attachment" "file" "http" "https" "pdf" "mol" "smol")))
    (with-temp-buffer
      (insert (org-roam-graph--dot (org-roam-graph--connected-component
                                    (org-roam-node-id NODE) DISTANCE)))
      (call-process-region (point-min) (point-max) org-roam-graph-executable t '(t nil) nil "-Tpdf")
      (buffer-string))))

;; use katex instead of mathjax
(setq org-html-mathjax-template "
<link rel=stylesheet href=katex/katex.min.css>
<script defer src=katex/katex.min.js></script>
<script defer src=katex/contrib/mhchem.min.js></script>
<script defer src=katex/contrib/copy-tex.min.js></script>
<script defer src=katex/contrib/auto-render.min.js
    onload=\"renderMathInElement(document.body);\"></script>")

;; org-publish roam project
(setq org-publish-project-alist
      `(("roam-notes"
         :base-directory ,(concat org-directory "/roam/")
         :base-extension "org"
         :publishing-directory ,(concat org-directory "/roam_html/")
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-self-link-headlines t
         :headline-levels 4
         :section-numbers 2
         :auto-preamble t
         :preparation-function (lambda (args)
                                 (org-roam-db-sync)
                                 (f-touch (concat org-roam-directory "index.org"))))
        ("roam-static"
         :base-directory ,(concat org-directory "/roam/")
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
         :publishing-directory ,(concat org-directory "/roam_html/")
         :recursive t
         :publishing-function org-publish-attachment)
        ("org" :components ("roam-notes" "roam-static"))))

(setq org-time-stamp-custom-formats '("%-e. %b %Y (%a)" . "%-e. %b %Y (%a) %H:%M"))
(add-hook 'org-export-before-processing-functions (lambda (_backend) (setq-local org-display-custom-times t)))

(setq org-agenda-buffer-name "Org Agenda")

;; fontification
(setq org-highlight-latex-and-related '(latex entities))
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

;; custom links

;; exports SMILES-format molecule definition as svg
(org-link-set-parameters "mol" :export #'mol-export)
(defun mol-export (link description format _)
  (pcase format
    ((or `html `md)
     (shell-command-to-string
      ;; -xb none : no background
      ;; -xx : omit XML declaration
      ;; -xe : embed molecule as CML
      (format "obabel -:\"%s\" -osvg -xb none -xx -xe 2>/dev/null" link)))))

;; same as mol, but in aside
(org-link-set-parameters "smol" :export #'smol-export)
(defun smol-export (link description format _)
  (pcase format
    ((or `html `md)
     (format "<span class=\"aside\">%s</span>"
             (shell-command-to-string
              ;; -xb none : no background
              ;; -xx : omit XML declaration
              ;; -xe : embed molecule as CML
              ;; -xt : thicker lines
              ;; --px : image size
              (format "obabel -:\"%s\" -osvg -xb none -xx -xe -xt --px 100 2>/dev/null" link))))))
