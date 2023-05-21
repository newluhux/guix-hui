(use-modules (gnu) (guix profiles))

(load "../packages/dict.scm")

(define-public %dict-packages
  (list ustardict stardict-ecdict))

(packages->manifest %dict-packages)
