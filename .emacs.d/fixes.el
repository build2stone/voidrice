;; Make font faces extend after emacs 27
;; Taken from https://gitlab.com/andreyorst/dotfiles

(when (>= emacs-major-version 27)
  (with-eval-after-load 'org
    (dolist (face '(org-block
                    org-block-begin-line
                    org-block-end-line
                    org-level-1
                    org-quote))
      (set-face-attribute face nil :extend t)))
  (with-eval-after-load 'magit
    (dolist (face '(magit-diff-hunk-heading
                    magit-diff-hunk-heading-highlight
                    magit-diff-hunk-heading-selection
                    magit-diff-hunk-region
                    magit-diff-lines-heading
                    magit-diff-lines-boundary
                    magit-diff-conflict-heading
                    magit-diff-added
                    magit-diff-removed
                    magit-diff-our
                    magit-diff-base
                    magit-diff-their
                    magit-diff-context
                    magit-diff-added-highlight
                    magit-diff-removed-highlight
                    magit-diff-our-highlight
                    magit-diff-base-highlight
                    magit-diff-their-highlight
                    magit-diff-context-highlight
                    magit-diff-whitespace-warning
                    magit-diffstat-added
                    magit-diffstat-removed
                    magit-section-heading
                    magit-section-heading-selection
                    magit-section-highlight
                    magit-section-secondary-heading
                    magit-diff-file-heading
                    magit-diff-file-heading-highlight
                    magit-diff-file-heading-selection))
      (set-face-attribute face nil :extend t)))
  (with-eval-after-load 'ediff
    (dolist (face '(ediff-current-diff-A
                    ediff-current-diff-Ancestor
                    ediff-current-diff-B
                    ediff-current-diff-C
                    ediff-even-diff-A
                    ediff-even-diff-Ancestor
                    ediff-even-diff-B
                    ediff-even-diff-C
                    ediff-fine-diff-A
                    ediff-fine-diff-Ancestor
                    ediff-fine-diff-B
                    ediff-fine-diff-C
                    ediff-odd-diff-A
                    ediff-odd-diff-Ancestor
                    ediff-odd-diff-B
                    ediff-odd-diff-C))
      (set-face-attribute face nil :extend t)))
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil :extend t))
  (with-eval-after-load 'faces
    (dolist (face '(region
                    secondary-selection))
      (set-face-attribute face nil :extend t)))
  (with-eval-after-load 'markdown-mode
    (dolist (face '(markdown-code-face
                    markdown-pre-face))
      (set-face-attribute face nil :extend t))))
