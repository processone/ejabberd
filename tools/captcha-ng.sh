#!/bin/bash

# Copyright © 2021 Adrien Bourmault (neox@os-k.eu)
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This script is an example captcha script.
# It takes the text to recognize in the captcha image as a parameter.
# It return the image binary as a result. ejabberd support PNG, JPEG and GIF.

# The whole idea of the captcha script is to let server admins adapt it to
# their own needs. The goal is to be able to make the captcha generation as
# unique as possible, to make the captcha challenge difficult to bypass by
# a bot.
# Server admins are thus supposed to write and use their own captcha generators.

# This script relies on ImageMagick.
# It is NOT compliant with ImageMagick forks like GraphicsMagick.

INPUT=$1

TRANSFORMATIONS=(INTRUDER SUM)
DIGIT=(zero one two three four five six seven eight nine ten)

if test -n "${BASH_VERSION:-''}" ; then
    get_random ()
    {  
	R=$RANDOM
    }
else
    for n in $(od -A n -t u2 -N 64 /dev/urandom); do RL="$RL$n "; done
    get_random ()
    {  
	R=${RL%% *}
	RL=${RL#* }
    }
fi

INTRUDER()
{
NUMBERS=$(echo "$INPUT" | grep -o . | tr '\n' ' ')
SORTED_UNIQ_NUM=$(echo "${NUMBERS[@]}" | sort -u | tr '\n' ' ')
SORT_RANDOM_CMD="$( ( echo x|sort -R >&/dev/null && echo "sort -R" ) || ( echo x|shuf >&/dev/null && echo shuf ) || echo cat)"
RANDOM_DIGITS=$(echo 123456789 | grep -o . | eval "$SORT_RANDOM_CMD" | tr '\n' ' ')
INTRUDER=-1

for i in $RANDOM_DIGITS
do
    if [[ ! " ${SORTED_UNIQ_NUM[@]} " =~ ${i} ]]; then
        INTRUDER=$i
        break
    fi
done

# Worst case
if [[ $INTRUDER -eq "-1" ]]
then
    printf "Type %s \n without changes" "$INPUT"
    return
fi

for num in ${NUMBERS}
do
    get_random
    R=$((R % 100))

    if [[ $R -lt 60 ]]; then
        NEWINPUT=${NEWINPUT}${num}${INTRUDER}
    else
        NEWINPUT=${NEWINPUT}${num}
    fi
done

get_random
R=$((R % 100))

if [[ $R -lt 50 ]]; then
    printf "Type %s by\n  deleting the %s" "$NEWINPUT" "${DIGIT[$INTRUDER]}"
else
    printf "Enter %s by\n  removing the %s" "$NEWINPUT" "${DIGIT[$INTRUDER]}"
fi
}


SUM()
{
get_random
RA=$((R % 100))

if [[ $((INPUT % 2)) -eq 0 ]]; then
    A=$((INPUT - RA))
    B=$RA
else
    B=$((INPUT - RA))
    A=$RA
fi

get_random
R=$((R % 100))

if [[ $R -lt 25 ]]; then
    printf "Type the result\n  of %s + %s" "$A" "$B"
elif [[ $R -lt 50 ]]; then
    printf "SUMx\n  %s and %s" "$A" "$B"
elif [[ $R -lt 75 ]]; then
    printf "Add\n  %s and %s" "$A" "$B"
else
    printf "Enter the result\n  of %s + %s" "$A" "$B"
fi
}


get_random

RAND_ITALIC=$((R % 25))
get_random

RAND_ANGLE=$((R % 3))
get_random

RAND_INDEX=$((R % ${#TRANSFORMATIONS[@]}))

convert -size 300x60 xc:none -pointsize 20 \
\( -clone 0 -fill black \
-stroke black -strokewidth 1 \
-annotate "${RAND_ANGLE}x${RAND_ITALIC}+0+0" "\n $(${TRANSFORMATIONS[$RAND_INDEX]})" \
-roll +"$ROLL_X"+0 \
-wave "$WAVE1_AMPLITUDE"x"$WAVE1_LENGTH" \
-roll -"$ROLL_X"+0 \) \
-flatten -crop 300x60 +repage -quality 500 -depth 11 png:-
