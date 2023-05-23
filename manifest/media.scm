(use-modules (gnu) (guix profiles))
(use-package-modules image-viewers xdisorg video linux)

(load "../packages/telegram.scm")

(define-public %media-packages
  (list
   feh scrot ffmpeg alsa-utils tgs2png))

(packages->manifest %media-packages)
