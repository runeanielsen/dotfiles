#!/usr/bin/env bash

VIDEOS_PATH=$HOME/nas/videos/

prog=$(exa $VIDEOS_PATH -R | rg '\.webm|\.mp4|\.mkv')

cmd=$(dmenu -l 20 -i -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$prog")

# Exit if cmd output is empty
if [ -z "$cmd" ]
then
    exit 1
fi

VIDEO=$(find $VIDEOS_PATH -name "$cmd")
$(mpv "$VIDEO")
