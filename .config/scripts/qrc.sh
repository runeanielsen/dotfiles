#!/bin/bash

declare code=$1

qrencode -m 5 -o /tmp/qrcode.png $code

feh /tmp/qrcode.png
