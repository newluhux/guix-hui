(use-modules (gnu) (guix profiles))
(use-package-modules xorg wm xdisorg qt kde-frameworks)


(define-public %xorg-packages
  (list
   cwm xterm xset xsetroot xkbset xrdb
   qt5ct breeze-icons))

(packages->manifest %xorg-packages)
