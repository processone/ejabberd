#!/bin/bash

# To start formatting a file, add a line that contains:
#   @format-begin
# Formatting in that file can later be disabled adding another line with:
#   @format-end
#
# It can be reenabled again later in the file.
#
# Finally, call: make format

REBAR=$1

FORMAT()
{
FPATH=$1
ERLS=$(git grep --name-only @format-begin "$FPATH"/)

for ERL in $ERLS; do
    perl -n -e 'sub o { open(OUT, ">", sprintf("%s-format-%02d", $f, $n++));}; BEGIN{($f)=@ARGV;o()}; o() if /\@format-/; print OUT $_;' $ERL
done

EFMTS=$(find "$FPATH"/*-format-* -type f -exec grep --files-with-matches "@format-begin" '{}' ';')
EFMTS2=""
for EFMT in $EFMTS; do
    EFMTS2="$EFMTS2 --files $EFMT"
done
$REBAR format $EFMTS2

for ERL in $ERLS; do
    SPLITS=$(find $ERL-format-* -type f)
    rm $ERL
    for SPLIT in $SPLITS; do
         cat $SPLIT >> $ERL
         rm $SPLIT
    done
done
}

FORMAT src
FORMAT test
