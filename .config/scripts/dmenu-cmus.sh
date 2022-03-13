#!/usr/bin/env bash

MUSIC_PATH=$HOME/Nas/Music/

prog="$(ls $MUSIC_PATH)"

cmd=$(dmenu -l 20 -i -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$prog")

# Exit if cmd output is empty
if [ -z "$cmd" ]
then
    exit 1
fi

$(cmus-remote --pause)

BAND_PATH=$MUSIC_PATH$cmd
$(cmus-remote --clear -q "$BAND_PATH")

# The sleep is needed so cmus can load the tracks before playing next
sleep 1s

$(cmus-remote --next)
