(use-modules (gnu) (guix profiles))
(use-package-modules fonts fontutils)

(define-public %font-packages
  (list
   fontconfig font-gnu-unifont font-google-noto-emoji))

(packages->manifest %font-packages)
