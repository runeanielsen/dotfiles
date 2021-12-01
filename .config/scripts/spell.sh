#!/usr/bin/env bash

# Use `fzf` against system dictionary
cat /usr/share/dict/words | fzf --preview 'wn {} -over | fold' --preview-window=up:80%
