#!/usr/bin/env bash

PLAYING=$(cmus-remote -Q | grep -w 'status')

echo $PLAYING
if [[ $PLAYING == "status playing" ]]
then
    $(cmus-remote -U)
else
    $(cmus-remote -p)
fi
