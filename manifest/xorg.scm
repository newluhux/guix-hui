(use-modules (gnu) (guix profiles))
(use-package-modules xorg wm xdisorg)


(define-public %xorg-packages
  (list
   cwm xterm xset xsetroot xkbset xrdb))

(packages->manifest %xorg-packages)
