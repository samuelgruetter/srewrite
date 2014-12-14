#!/bin/sh

if [ ! -d "$1" ]; then
  echo "First argument should be dotty tests directory"
  exit 1
fi

posCount=0
count=0
negCount=0

# directories:
for f in $1/untried/pos/*.scala ; do
  echo "===$f==="
  if dotc $f ; then
    to=$1/pos
    echo "pos: mv $f $to"
    mv $f $to
    posCount=`expr $posCount + 1`
  else
    echo "neg"
    negCount=`expr $negCount + 1`
  fi
  count=`expr $count + 1`
  
  # if [ $count = 10 ] ; then
  #  break
  # fi
done

echo "files: pos:$posCount, neg:$negCount, total: $count"

