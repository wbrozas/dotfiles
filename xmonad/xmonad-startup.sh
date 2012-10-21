#!/bin/bash
# commands
TRAY="/usr/bin/stalonetray --geometry 13x1+700+0 --max-geometry 13x1+700+0 --background #161616 --icon-size 16 --icon-gravity NE --kludges=force_icons_size"
NET="/usr/bin/nm-applet"
POWER="/usr/bin/gnome-power-manager"
VOLUME="/usr/bin/gnome-volume-control-applet"

xmodmap ~/.xmodmap
xrdb -merge ~/.Xdefaults &
xpmroot /home/kyle/.xmonad/lambda1024x600.xpm &

(sleep 1 && $TRAY)&
(sleep 3 && $POWER )&
(sleep 3 && $NET )&
(sleep 3 && $VOLUME )&
