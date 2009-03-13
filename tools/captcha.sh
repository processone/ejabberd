#!/bin/sh

SIGN=$(($RANDOM % 2))

R1=$(($RANDOM % 20))
R2=$(($RANDOM % 10 + 40))

if [ $SIGN -eq "0" ]; then
    S1=$(( -1*($RANDOM % 20 + 50) ))
    S2=$(( $RANDOM % 20 + 50 ))
else
    S2=$(( -1*($RANDOM % 20 + 50) ))
    S1=$(( $RANDOM % 20 + 50 ))
fi

convert -size 140x60 xc:white \
    -pointsize 30 -draw "text 20,30 '$1'" \
    -roll -$R2+$R1 -swirl $S1 \
    -roll +$R2-$R1 -swirl $S2 \
    +repage -resize 120x60 \
    -quality 90 -depth 8 png:-
