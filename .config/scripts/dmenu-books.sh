#!/usr/bin/env bash

BOOKS_PATH=$HOME/nas/books/

PROG=$(exa $BOOKS_PATH -R | rg '\.pdf|\.epub')

CMD=$(dmenu -l 20 -i -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$PROG")

# Exit if cmd output is empty
if [ -z "$CMD" ]
then
    exit 1
fi

BOOK_PATH="$BOOKS_PATH$CMD"
$(zathura "${BOOK_PATH}")
