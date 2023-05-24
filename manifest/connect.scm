(use-modules (gnu) (guix profiles))
(use-package-modules ssh terminals admin screen tmux)

(load "../packages/embedded.scm")

(define-public %connect-packages
  (list
   openssh picocom lrzsz ek eksw screen tmux gkermit ukermit ckermit))

(packages->manifest %connect-packages)
