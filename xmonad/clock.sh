#!/bin/bash

while true; do
  tim=`date +'%a %b %d %l:%M%p'`
#  bat=`~/.xmonad/battery.sh`
#  echo "$bat | $tim"
  echo "$tim"
  sleep 30
done
