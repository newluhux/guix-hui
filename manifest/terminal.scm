(use-modules (gnu) (guix profiles))
(use-package-modules ncurses)

(define-public %terminal-packages
  (list ncurses))

(packages->manifest %terminal-packages)
