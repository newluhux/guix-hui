(use-modules (gnu) (guix profiles))
(use-package-modules emacs emacs-xyz)

(define-public %emacs-packages
  (list
   emacs-next emacs-company emacs-lsp-mode emacs-yasnippet
   emacs-yasnippet-snippets emacs-lsp-ui emacs-geiser emacs-geiser-guile
   emacs-rime emacs-telega emacs-magit emacs-ccls emacs-ivy emacs-paredit
   emacs-dts-mode))

(packages->manifest %emacs-packages)
