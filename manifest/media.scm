(use-modules (gnu) (guix profiles))
(use-package-modules image-viewers xdisorg video linux)


(define-public %media-packages
  (list
   feh scrot ffmpeg alsa-utils))

(packages->manifest %media-packages)
