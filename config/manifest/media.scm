(use-modules (gnu) (guix profiles) (hui packages telegram))
(use-package-modules image-viewers xdisorg video linux kde-multimedia)

(define-public %media-packages
  (list
   feh scrot ffmpeg alsa-utils tgs2png k3b))

(packages->manifest %media-packages)
