(use-modules (gnu) (guix profiles))
(use-package-modules radio)

(define-public %radio-packages
  (list gqrx rtl-sdr dump1090))

(packages->manifest %radio-packages)
