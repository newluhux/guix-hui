(use-modules (gnu) (guix profiles))
(use-package-modules xorg wm xdisorg qt kde-frameworks fcitx5)


(define-public %xorg-packages
  (list
   cwm xterm xset xsetroot xkbset xrdb
   qt5ct breeze-icons
   fcitx5 fcitx5-rime fcitx5-gtk fcitx5-qt fcitx5-gtk4 fcitx5-configtool))

(packages->manifest %xorg-packages)
