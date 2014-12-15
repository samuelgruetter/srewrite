#!/bin/sh

if [ ! -d "$1" ]; then
  echo "First argument should be dotty tests directory"
  exit 1
fi

count=0

for f in $1/untried/pos/* ; do
  # echo "===$f==="
  
  count=`expr $count + 1`
  
  FILENAME=${f##*/}
  NOEXT=${FILENAME%\.*}
  
  if [ -e $1/pos/$NOEXT.scala ] ; then
    echo "mv $f $1/pos"
    mv $f $1/pos
  fi
  
  # if [ $count = 20 ] ; then
  # break
  # fi
done

# echo "files: pos:$posCount, neg:$negCount, total: $count"

