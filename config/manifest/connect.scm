(use-modules (gnu) (guix profiles) (hui packages embedded))
(use-package-modules ssh terminals admin screen tmux networking)

(define-public %connect-packages
  (list
   openssh picocom lrzsz ek eksw screen tmux gkermit ukermit ckermit iperf))

(packages->manifest %connect-packages)
