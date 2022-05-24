#!/usr/bin/env bash

artist=$(cmus-remote -Q | sed -n -e 's/^.*tag artist //p')
song=$(cmus-remote -Q | sed -n -e 's/^.*tag title //p')
position=$(cmus-remote -Q | sed -n -e 's/^.*position //p')
duration=$(cmus-remote -Q | sed -n -e 's/^.*duration //p')

truncate_text() {
    local text=$1
    local max_length=$2

    if [[ ${#text} -ge max_length ]]
    then
        text=${text:0:($max_length-1)}
        text="${text%%*( )}..."
    fi

    truncate_text_result=$text
}

make_into_time() {
    local min=`expr $1 / 60`
    local sec=`expr $1 % 60`

    if [ `expr length $min` -eq 1 ]
    then
        local min="0"$min
    fi

    if [ `expr length $sec` -eq 1 ]
    then
        local sec="0"$sec
    fi

    make_into_time_result=$min:$sec
}

if [ -z "$song" ]
then
    echo ""
else
    make_into_time $position
    position_time=$make_into_time_result

    make_into_time $duration
    duration_time=$make_into_time_result

    truncate_text "$artist" 30
    artist=$truncate_text_result

    truncate_text "$song" 30
    song=$truncate_text_result

    echo "$artist : $song ($position_time/$duration_time)"
fi
