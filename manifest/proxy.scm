(use-modules (gnu) (guix profiles))
(use-package-modules networking)

(load "../packages/proxy.scm")

(define-public %proxy-packages
  (list proxychains-ng go-github-com-dreamacro-clash))

(packages->manifest %proxy-packages)
