#!/bin/bash

IMG_PATH=/home/notation/screenshots/

prog="
1.quick_fullscreen
2.delayed_fullscreen
3.section
"

cmd=$(dmenu -l 20 -nf '#000000' -nb '#FFFFFF' -sf '#FFFFFF' -sb '#000000' -p 'Choose Screenshot Type' <<< "$prog")

cd $IMG_PATH
case ${cmd%% *} in
    1.quick_fullscreen)     scrot -d 1;;
    2.delayed_fullscreen)   scrot -d 4;;
    3.section)              scrot -s;;
    *) exec "'${cmd}'";
esac
