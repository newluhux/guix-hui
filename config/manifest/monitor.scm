(use-modules (gnu) (guix profiles))
(use-package-modules admin networking)

(define-public %monitor-packages
  (list htop bmon iftop tcpdump))

(packages->manifest %monitor-packages)
