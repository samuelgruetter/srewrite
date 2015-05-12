#!/bin/sh

pluginPath=~/".ivy2/local/org.scala-lang.plugins/srewriteplugin_2.10/0.1.0/jars/srewriteplugin_2.10.jar"
pluginOptions="-P:srewriteplugin:oversrc"

if [ ! -d "$1" ]; then
  echo "First argument should be a directory containing single-file tests and folders containing multifile-tests"
  exit 1
fi

# directories:
for d in $1/*/ ; do
  echo "===$d==="
  ./run-srewrite.sh $d
done

# files:
for f in $1/*.scala ; do
  echo "===$f==="
  scalac -J-Xmx2g -Yrangepos -Ystop-before:patmat "-Xplugin:$pluginPath" "$pluginOptions" $f
done

