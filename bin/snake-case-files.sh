#!/bin/bash

find . -name "**" -exec bash -c 'mv "$0" "${0// /_}"' {} \;
for f in `find`; do mv -v "$f" "`echo $f | tr '[A-Z]' '[a-z]'`"; done
