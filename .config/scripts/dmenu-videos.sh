#!/usr/bin/env bash

VIDEOS_PATH=$HOME/nas/videos/

PROG=$(find -D tree $VIDEOS_PATH | rg '\.webm|\.mp4|\.mkv') 
# Here we remove videos_path from output, for improved layout.
PROG=${PROG//$VIDEOS_PATH/}

CMD=$(dmenu -l 20 -i -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$PROG")

# Exit if cmd output is empty
if [ -z "$CMD" ]
then
    exit 1
fi

VIDEO="$VIDEOS_PATH$CMD"
$(mpv "${VIDEO}")
