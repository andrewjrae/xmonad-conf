#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#Set your native resolution IF it does not exist in xrandr
#More info in the script
#run $HOME/.xmonad/scripts/set-screen-resolution-in-virtualbox.sh

#cursor active at boot
xsetroot -cursor_name left_ptr &

# update monitors
autorandr --change

#starting utility applications at boot time
/etc/local/scripts/polybar_launch.sh
/etc/local/scripts/wacom-setup.sh
# run nm-applet &
# run pamac-tray &
# run xfce4-power-manager &
# run volumeicon &
# numlockx on &
# blueberry-tray &
run unclutter &
picom --config $HOME/.xmonad/scripts/picom.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &

#starting user applications at boot time
nitrogen --restore &
run /usr/bin/emacs --daemon &
