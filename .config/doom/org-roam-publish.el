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

;; add org-roam files to agenda
(add-to-list 'org-agenda-files (concat org-directory "/roam/"))

(defun my-synchronous-org-roam-graph-svg (NODE DISTANCE)
  "Returns an svg-format graph of the surroundings of NODE, up to DISTANCE (DISTANCE 0 graphs everything)
Replaces org-protocol links with relative links to exported html files."
  (require 'org-roam-graph)
  (let* ((org-roam-graph-link-hidden-types '("attachment" "file" "http" "https" "pdf" "mol" "smol"))
         (current-file (buffer-file-name))
         (org-roam-graph-link-builder
          (lambda (node)
            (let ((node (org-roam-populate node)) ;; fetch node info from database
                  (exported-file (when (not (string= current-file (org-roam-node-file node)))
                                   (concat (file-name-base (org-roam-node-file node)) ".html"))))
              (if (= 0 (org-roam-node-level node))
                  ;; level 0 node = link to whole file
                  exported-file
                ;; level >0 node = link to heading in file
                (concat exported-file "#" (zz/make-id-for-title (org-roam-node-title node))))))))

    (with-temp-buffer
      (insert (org-roam-graph--dot (org-roam-graph--connected-component
                                    (org-roam-node-id NODE) DISTANCE)))
      (call-process-region nil nil org-roam-graph-executable t '(t nil) nil "-Tsvg")

      (let*
          ((svggraph (libxml-parse-xml-region (point-min) (point-max)))
           (svgtag (dom-by-tag svggraph 'svg)))
        (dom-set-attribute svgtag 'width "100%")
        (dom-set-attribute svgtag 'height "auto")
        (with-temp-buffer
          (xml-print svgtag)
          (buffer-string))))))

(defun my-synchronous-org-roam-graph-pdf (NODE DISTANCE)
  "Returns a pdf-format graph of the surroundings of NODE, up to DISTANCE (DISTANCE 0 graphs everything)
TODO: convert links similarly to svg version"
  (require 'org-roam-graph)
  (let* ((org-roam-graph-link-hidden-types '("attachment" "file" "http" "https" "pdf" "mol" "smol")))
    (with-temp-buffer
      (insert (org-roam-graph--dot (org-roam-graph--connected-component
                                    (org-roam-node-id NODE) DISTANCE)))
      (call-process-region nil nil org-roam-graph-executable t '(t nil) nil "-Tpdf")
      (buffer-string))))

;; use katex instead of mathjax
(setq org-html-mathjax-template "
<link rel=stylesheet href=katex/katex.min.css>
<script defer src=katex/katex.min.js></script>
<script defer src=katex/contrib/mhchem.min.js></script>
<script defer src=katex/contrib/copy-tex.min.js></script>
<script defer src=katex/contrib/auto-render.min.js
    onload=\"renderMathInElement(document.body);\"></script>")
