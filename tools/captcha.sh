#!/bin/sh

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

for n in $(od -A n -t u2 -N 48 /dev/urandom); do RL="$RL$n "; done
get_random ()
{
    R=${RL%% *}
    RL=${RL#* }
}

get_random
WAVE1_AMPLITUDE=$((2 + R % 5))
get_random
WAVE1_LENGTH=$((50 + R % 25))
get_random
WAVE2_AMPLITUDE=$((2 + R % 5))
get_random
WAVE2_LENGTH=$((50 + R % 25))
get_random
WAVE3_AMPLITUDE=$((2 + R % 5))
get_random
WAVE3_LENGTH=$((50 + R % 25))
get_random
W1_LINE_START_Y=$((10 + R % 40))
get_random
W1_LINE_STOP_Y=$((10 + R % 40))
get_random
W2_LINE_START_Y=$((10 + R % 40))
get_random
W2_LINE_STOP_Y=$((10 + R % 40))
get_random
W3_LINE_START_Y=$((10 + R % 40))
get_random
W3_LINE_STOP_Y=$((10 + R % 40))

get_random
B1_LINE_START_Y=$((R % 40))
get_random
B1_LINE_STOP_Y=$((R % 40))
get_random
B2_LINE_START_Y=$((R % 40))
get_random
B2_LINE_STOP_Y=$((R % 40))
#B3_LINE_START_Y=$((R % 40))
#B3_LINE_STOP_Y=$((R % 40))

get_random
B1_LINE_START_X=$((R % 20))
get_random
B1_LINE_STOP_X=$((100 + R % 40))
get_random
B2_LINE_START_X=$((R % 20))
get_random
B2_LINE_STOP_X=$((100 + R % 40))
#B3_LINE_START_X=$((R % 20))
#B3_LINE_STOP_X=$((100 + R % 40))

get_random
ROLL_X=$((R % 40))

convert -size 180x60 xc:none -pointsize 40 \
	\( -clone 0 -fill white \
	-stroke black -strokewidth 4 -annotate +0+40 "$INPUT" \
	-stroke white -strokewidth 2 -annotate +0+40 "$INPUT" \
	-roll +$ROLL_X+0 \
	-wave "$WAVE1_AMPLITUDE"x"$WAVE1_LENGTH" \
	-roll -$ROLL_X+0 \) \
	\( -clone 0 -stroke black \
	-strokewidth 1 -draw \
	"line $B1_LINE_START_X,$B1_LINE_START_Y $B1_LINE_STOP_X,$B1_LINE_STOP_Y" \
	-strokewidth 1 -draw \
	"line $B2_LINE_START_X,$B2_LINE_START_Y $B2_LINE_STOP_X,$B2_LINE_STOP_Y" \
	-wave "$WAVE2_AMPLITUDE"x"$WAVE2_LENGTH" \) \
	\( -clone 0 -stroke white \
	-strokewidth 2 -draw "line 0,$W1_LINE_START_Y 140,$W1_LINE_STOP_Y" \
	-strokewidth 2 -draw "line 0,$W2_LINE_START_Y 140,$W2_LINE_STOP_Y" \
	-strokewidth 2 -draw "line 0,$W3_LINE_START_Y 140,$W3_LINE_STOP_Y" \
	-wave "$WAVE3_AMPLITUDE"x"$WAVE3_LENGTH" \) \
	-flatten -crop 140x60 +repage -quality 90 -depth 8 png:-
