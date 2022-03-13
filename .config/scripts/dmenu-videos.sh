#!/usr/bin/env bash

VIDEOS_PATH=$HOME/nas/videos/

PROG=$(exa $VIDEOS_PATH -R | rg '\.webm|\.mp4|\.mkv')

CMD=$(dmenu -l 20 -i -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$PROG")

# Exit if cmd output is empty
if [ -z "$CMD" ]
then
    exit 1
fi

VIDEO_PATH="$VIDEOS_PATH$CMD"

$(mpv "${VIDEO_PATH}")
