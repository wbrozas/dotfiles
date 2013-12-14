#!/bin/bash
# commands
TRAY="/usr/bin/stalonetray --geometry 13x1+1585+0 --max-geometry 13x1+1585+0 --background #161616 --icon-size 16 --icon-gravity NE --kludges=force_icons_size"
NET="/usr/bin/nm-applet"
VOLUME="/usr/bin/pnmixer"

xmodmap ~/.xmodmap
xrdb -merge ~/.Xdefaults &

(sleep 1 && $TRAY)&
(sleep 3 && $NET)&
(sleep 3 && $VOLUME)&
