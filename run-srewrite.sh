#!/bin/sh

pluginPath=~/".ivy2/local/org.scala-lang.plugins/srewriteplugin_2.10/0.1.0/jars/srewriteplugin_2.10.jar"
pluginOptions="-P:srewriteplugin:oversrc"

if [ ! -d "$1" ]; then
  echo "First argument should be a directory"
  exit 1
fi

# Java & Scala files:
# sourceFiles=`find $1 \( -name '*.scala' -o -name '*.java' \) -printf ' %p'`

# Only Scala files:
sourceFiles=`find $1 -name '*.scala' -printf ' %p'`

# 2GB should be enough... it takes ~800MB for the scala standard lib
scalac -J-Xmx2g -Yrangepos -Ystop-before:patmat "-Xplugin:$pluginPath" "$pluginOptions" $sourceFiles

