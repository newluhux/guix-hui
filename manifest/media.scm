(use-modules (gnu) (guix profiles))
(use-package-modules image-viewers xdisorg video linux kde-multimedia)

(load "../packages/telegram.scm")

(define-public %media-packages
  (list
   feh scrot ffmpeg alsa-utils tgs2png k3b))

(packages->manifest %media-packages)
