(use-modules (gnu) (guix profiles) (hui packages proxy))
(use-package-modules networking)

(define-public %proxy-packages
  (list proxychains-ng go-github-com-dreamacro-clash))

(packages->manifest %proxy-packages)
