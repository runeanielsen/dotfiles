#!/usr/bin/bash

declare code=$1

qrencode -s 12 -o /tmp/qrcode.png $code

feh /tmp/qrcode.png
