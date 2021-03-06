#!/usr/bin/env bash

BOOKS_PATH=$HOME/nas/books/

prog=$(exa $BOOKS_PATH -R | rg '\.pdf|\.epub')

cmd=$(dmenu -l 20 -i -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$prog")

# Exit if cmd output is empty
if [ -z "$cmd" ]
then
    exit 1
fi

BOOK=$(find $BOOKS_PATH -name "$cmd")
$(zathura "$BOOK")
