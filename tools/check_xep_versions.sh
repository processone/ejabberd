#!/bin/bash

check_xep()
{
    xep=xep-$1
    int=$(echo $1 | sed 's/^0*//')
    [ -f $BASE/doc/$xep ] || curl -s -o $BASE/doc/$xep https://xmpp.org/extensions/$xep.html
    title=$(sed '/<title>/!d;s/.*<title>\(.*\)<\/title>.*/\1/' $BASE/doc/$xep)
    vsn=$(grep -A1 Version $BASE/doc/$xep | sed '/<dd>/!d;q' | sed 's/.*>\(.*\)<.*/\1/')
    imp=$(grep "{xep, $int," $BASE/src/* | sed "s/.*src\/\(.*\).erl.*'\([0-9.-]*\)'.*/\1 \2/")
    [ "$imp" == "" ] && imp="NA 0.0"
    echo "$title;$vsn;${imp/ /;}"
}

[ $# -eq 1 ] && BASE="$1" || BASE="$PWD"
[ -d $BASE/doc ] || mkdir $BASE/doc

for x_num in $(grep "{xep" $BASE/src/* | sed "s/,//" | awk '{printf("%04d\n", $2)}' | sort -u)
do
  check_xep $x_num
done
