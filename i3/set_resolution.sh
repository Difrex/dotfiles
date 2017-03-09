#!/bin/bash

HDMI2=`xrandr | grep 'HDMI2 connected'`

if [[ ! -z $HDMI2 ]]; then
    xrandr --output eDP1 --mode 1920x1200 --output HDMI2 --mode 1920x1200 --same-as eDP1
fi
