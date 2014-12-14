#!/bin/sh

if [ ! -d "$1" ]; then
  echo "First argument should be dotty tests directory"
  exit 1
fi

posCount=0
count=0
negCount=0

# directories:
for d in $1/untried/pos/*/ ; do
  echo "===$d==="
  if ./run-dotty.sh $d ; then
    to=$1/pos
    echo "pos: mv $d $to"
    mv $d $to
    posCount=`expr $posCount + 1`
  else
    echo "neg"
    negCount=`expr $negCount + 1`
  fi
  count=`expr $count + 1`
done

echo "directories: pos:$posCount, neg:$negCount, total: $count"

# files:
# for f in $1/untried/pos/*.scala ; do
#   echo "===$f==="
#   scalac -J-Xmx2g -Yrangepos -Ystop-before:patmat "-Xplugin:$pluginPath" "$pluginOptions" $f
# done

