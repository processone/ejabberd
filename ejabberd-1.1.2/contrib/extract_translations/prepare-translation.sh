#!/bin/bash

# Frontend for ejabberd's extract_translations.erl
# by Badlop
# last updated: 8 December 2005

while [ "$1" != "" ] 
do
    case "$1" in
        -help)
            echo "Options:"
            echo "  -lang LANGUAGE"
            echo "  -src FULL_PATH_EJABBERD"
            echo ""
            echo "Example:"
            echo "  ./prepare-translation.sh -lang es -src /home/admin/ejabberd"
            exit 0
            ;;
        -lang)
            # This is the language to be extracted
            LANGU=$2
            shift
            shift
            ;;
        -src)
            # This is the path to the ejabberd source dir
            EJA_DIR=$2
            shift
            shift
            ;;
        *)
            echo "unknown option: '$1 $2'"
            shift
            shift
            ;;
    esac
done

# Where is Erlang binary
ERL=`which erl`

EXTRACT_DIR=$EJA_DIR/contrib/extract_translations/
EXTRACT_ERL=extract_translations.erl
EXTRACT_BEAM=extract_translations.beam
SRC_DIR=$EJA_DIR/src
MSGS_DIR=$SRC_DIR/msgs
MSGS_FILE=$LANGU.msg
MSGS_FILE2=$LANGU.msg.translate
MSGS_PATH=$MSGS_DIR/$MSGS_FILE
MSGS_PATH2=$MSGS_DIR/$MSGS_FILE2

if !([[ -n $EJA_DIR ]])
then 
echo "ejabberd dir does not exist: $EJA_DIR"
fi

if !([[ -x $EXTRACT_BEAM ]])
then 
echo -n "Compiling extract_translations.erl: "
sh -c "cd $EXTRACT_DIR; $ERL -compile $EXTRACT_ERL"
fi

echo ""
echo -n "Extracting language strings for '$LANGU':"

echo -n " new..."
$ERL -pa $EXTRACT_DIR -noinput -noshell -s extract_translations -s init stop -extra $SRC_DIR $MSGS_PATH >$MSGS_PATH.new

echo -n " old..."
$ERL -pa $EXTRACT_DIR -noinput -noshell -s extract_translations -s init stop -extra -unused $SRC_DIR $MSGS_PATH >$MSGS_PATH.unused

cat $MSGS_PATH >$MSGS_PATH2

echo "" >>$MSGS_PATH2

cat $MSGS_PATH.new >>$MSGS_PATH2
rm $MSGS_PATH.new
echo "" >>$MSGS_PATH2

cat $MSGS_PATH.unused >>$MSGS_PATH2
rm $MSGS_PATH.unused

echo " ok"

echo ""
echo "Process completed."

echo ""
echo "  A new file has been created for you, with the current, the new and the deprecated strings:"
echo "  $MSGS_PATH2"
echo ""
echo "  At the end of that file you will find the strings you must update:"
echo "   - Untranslated strings are like this: {"March", ""}."
echo "     To translate the string, add the text inside the commas. Example:"
echo "     {"March", "Marzo"}."
echo "   - Old strings that are not used: "Woowoa""
echo "     Search the entire file for those strings and remove them"
echo ""
echo "  Once you have translated all the strings and removed all the old ones,"
echo "  rename the file to overwrite the previous one:"
echo "  $MSGS_PATH"
