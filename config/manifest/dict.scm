(use-modules (gnu) (guix profiles) (hui packages dict))

(define-public %dict-packages
  (list ustardict stardict-ecdict))

(packages->manifest %dict-packages)
