(use-modules (gnu) (guix profiles))
(use-package-modules networking)

(load "../packages/proxy.scm")

(define-public %proxy-packages
  (list proxychains-ng clash))

(packages->manifest %proxy-packages)
