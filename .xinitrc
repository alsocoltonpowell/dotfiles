#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# japanese
# export GTK_IM_MODULE='uim'
# export QT_IM_MODULE='uim'
# uim-xim &
# export XMODIFIERS='@im=uim'

#Background
nitrogen --restore &

#Nvidia-settings (disabled for now)
nvidia-settings -a [gpu:0]/GPUPowerMizerMode=1 &

# Compton fade in
compton -f -CG &

# Xscreensaver init
xscreensaver &

# Disable mouse acceleration
xset m 0/1 4 &

# Adjust screen positions
# xrandr --output DP-1 --auto --output HDMI-1 --auto --right-of DP-1

# kill pulseaudio to activate eq
# pulseaudio -k &
# pactl load-module module-equalizer-sink
# pactl load-module module-dbus-protocol

#start i3
exec i3
