;;; -*- lexical-binding: t; -*-

;; display

(setq org-agenda-buffer-name "Org Agenda")

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

;; export

(after! ob-ditaa
  (setq org-ditaa-jar-path (executable-find "ditaa")
        org-ditaa-jar-option ""
        org-babel-ditaa-java-cmd ""))

(setq org-time-stamp-custom-formats '("%-e. %b %Y (%a)" . "%-e. %b %Y (%a) %H:%M"))
(add-hook 'org-export-before-processing-functions (lambda (_backend) (setq-local org-display-custom-times t)))

;; link that exports SMILES-format molecule definition as svg
(org-link-set-parameters "mol" :export #'mol-export)
(defun mol-export (link description format _)
  (pcase format
    ((or `html `md)
     (shell-command-to-string
      ;; -xb none : no background
      ;; -xx : omit XML declaration
      ;; -xe : embed molecule as CML
      (format "obabel -:\"%s\" -osvg -xb none -xx -xe 2>/dev/null" link)))))

;; a smaller mol
(org-link-set-parameters "smol" :export #'smol-export)
(defun smol-export (link description format _)
  (pcase format
    ((or `html `md)
     (shell-command-to-string
      ;; -xb none : no background
      ;; -xx : omit XML declaration
      ;; -xe : embed molecule as CML
      ;; -xt : thicker lines
      ;; --px : image size
      (format "obabel -:\"%s\" -osvg -xb none -xx -xe -xt --px 100 2>/dev/null" link)))))
