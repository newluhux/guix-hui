(use-modules (gnu) (guix profiles))
(use-package-modules emacs emacs-xyz)

(define-public %emacs-packages
  (list
   emacs-no-x-toolkit emacs-company emacs-lsp-mode emacs-yasnippet
   emacs-yasnippet-snippets emacs-lsp-ui emacs-geiser emacs-geiser-guile
   emacs-rime emacs-telega emacs-magit emacs-ccls emacs-ivy))

(packages->manifest %emacs-packages)
