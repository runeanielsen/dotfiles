#!/usr/bin/env bash

playing=$(cmus-remote -Q | grep -w 'status')

echo "$playing"

if [[ $playing == "status playing" ]]
then
    eval "cmus-remote -U"
else
    eval "cmus-remote -p"
fi
