(use-modules (gnu) (guix profiles))
(use-package-modules games)

(define-public %game-packages
  (list warzone2100 cataclysm-dda nethack curseofwar))

(packages->manifest %game-packages)
