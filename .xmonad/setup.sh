#!/usr/bin/bash

set -e

git clone -b v0.17.1 "https://github.com/xmonad/xmonad" xmonad-git
git clone -b v0.17.1 "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
git clone -b 0.44.1 "https://codeberg.org/xmobar/xmobar.git" xmobar-git
