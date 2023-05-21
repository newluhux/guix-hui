(use-modules (gnu) (guix profiles))
(use-package-modules gnuzilla chromium web-browsers w3m curl)

(define-public %web-packages
  (list icecat ungoogled-chromium w3m links lynx curl))

(packages->manifest %web-packages)
