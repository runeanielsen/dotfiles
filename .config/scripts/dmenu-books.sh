#!/usr/bin/env bash

books_path=$HOME/nas/books/

prog=$(exa $books_path -R | rg '\.pdf|\.epub')

cmd=$(dmenu -l 20 -i -nf '#ffffff' -nb '#222222' -sf '#222222' -sb '#ffffff' <<< "$prog")

# Exit if cmd output is empty
if [ -z "$cmd" ]
then
    exit 1
fi

book_path="$books_path$cmd"
$(zathura "${book_path}")
