#!/bin/sh

eval `dbus-launch --sh-syntax --exit-with-session`

xset r rate 200 60
xsetroot -solid gray
setxkbmap -layout us -options ctrl:nocaps

fcitx5 -d

emacs --daemon

cwm
