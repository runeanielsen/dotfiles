#!/usr/bin/env bash

artist=$(cmus-remote -Q | sed -n -e 's/^.*tag artist //p')
song=$(cmus-remote -Q | sed -n -e 's/^.*tag title //p')
position=$(cmus-remote -Q | sed -n -e 's/^.*position //p')
duration=$(cmus-remote -Q | sed -n -e 's/^.*duration //p')

if [ -z "$song" ]
then
        echo ""
else
        position_time=`expr $position / 60`:`expr $position % 60`
        duration_time=`expr $duration / 60`:`expr $duration % 60`
        echo "$artist: $song ($position_time/$duration_time)"
fi
