#!/usr/bin/env bash

cat /usr/share/dict/words | fzf --preview 'wn {} -over | fold' --preview-window=up:80%
