#!/usr/bin/env bash

IMG_PATH=~/screenshots/

prog="1.quick_fullscreen
2.delayed_fullscreen
3.section"

cmd=$(dmenu -l 20 -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$prog")

cd $IMG_PATH
case ${cmd%% *} in
    1.quick_fullscreen)     scrot -d 1;;
    2.delayed_fullscreen)   scrot -d 4;;
    3.section)              scrot -s;;
    *) exec "'${cmd}'";
esac
