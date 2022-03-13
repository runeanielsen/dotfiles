#!/usr/bin/env bash

playing=$(cmus-remote -Q | grep -w 'status')

echo $playing
if [[ $playing == "status playing" ]]
then
    $(cmus-remote -U)
else
    $(cmus-remote -p)
fi
