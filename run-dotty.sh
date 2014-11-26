#!/bin/sh

if [ ! -d "$1" ]; then
  echo "First argument should be a directory"
  exit 1
fi

# Java & Scala files:
sourceFiles=`find $1 \( -name '*.scala' -o -name '*.java' \) -printf ' %p'`

# Only Scala files:
# sourceFiles=`find $1 -name '*.scala' -printf ' %p'`

# 2GB should be enough...
dotc -J-Xmx2g $sourceFiles

