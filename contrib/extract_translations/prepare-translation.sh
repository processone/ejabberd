#!/bin/bash

# Frontend for ejabberd's extract_translations.erl
# by Badlop

prepare_dirs ()
{
	# Where is Erlang binary
	ERL=`which erl`

	EJA_DIR=`pwd`/../..
	EXTRACT_DIR=$EJA_DIR/contrib/extract_translations/
	EXTRACT_ERL=extract_translations.erl
	EXTRACT_BEAM=extract_translations.beam
	SRC_DIR=$EJA_DIR/src
	MSGS_DIR=$SRC_DIR/msgs

	if !([[ -n $EJA_DIR ]])
	then 
	echo "ejabberd dir does not exist: $EJA_DIR"
	fi

	if !([[ -x $EXTRACT_BEAM ]])
	then 
	echo -n "Compiling extract_translations.erl: "
	sh -c "cd $EXTRACT_DIR; $ERL -compile $EXTRACT_ERL"
	echo "ok"
	fi
}

extract_lang ()
{
	MSGS_FILE=$1
	MSGS_FILE2=$MSGS_FILE.translate
	MSGS_PATH=$MSGS_DIR/$MSGS_FILE
	MSGS_PATH2=$MSGS_DIR/$MSGS_FILE2

	echo -n "Extracting language strings for '$MSGS_FILE':"

	echo -n " new..."
	cd $SRC_DIR
	$ERL -pa $EXTRACT_DIR -noinput -noshell -s extract_translations -s init stop -extra . $MSGS_PATH >$MSGS_PATH.new
	sed -e 's/^% \.\//% /g;' $MSGS_PATH.new > $MSGS_PATH.new2
	mv $MSGS_PATH.new2 $MSGS_PATH.new 

	echo -n " old..."
	$ERL -pa $EXTRACT_DIR -noinput -noshell -s extract_translations -s init stop -extra -unused . $MSGS_PATH >$MSGS_PATH.unused
	find_unused_full $MSGS_FILE $MSGS_FILE.unused

	echo "" >$MSGS_PATH2
	echo "    ***** Translation file for ejabberd ***** " >>$MSGS_PATH2
	echo "" >>$MSGS_PATH2

	echo "" >>$MSGS_PATH2
	echo "    *** New strings: Can you please translate them? *** " >>$MSGS_PATH2
	cat $MSGS_PATH.new >>$MSGS_PATH2

	echo "" >>$MSGS_PATH2
	echo "" >>$MSGS_PATH2
	echo "    *** Unused strings: They will be removed automatically *** " >>$MSGS_PATH2
	cat $MSGS_PATH.unused.full >>$MSGS_PATH2

	echo "" >>$MSGS_PATH2
	echo "" >>$MSGS_PATH2
	echo "    *** Already translated strings: you can also modify any of them if you want *** " >>$MSGS_PATH2
	echo "" >>$MSGS_PATH2
	cat $MSGS_PATH.old_cleaned >>$MSGS_PATH2

	echo " ok"

	rm $MSGS_PATH.new
	rm $MSGS_PATH.old_cleaned
	rm $MSGS_PATH.unused.full
}

extract_lang_all ()
{
	cd $MSGS_DIR
	for i in *.msg; do 
		extract_lang $i;
	done

	echo -e "File\tMissing\tLanguage\t\tLast translator"
	echo -e "----\t-------\t--------\t\t---------------"
	cd $MSGS_DIR
	for i in *.msg; do 
		MISSING=`cat $i.translate | grep "\", \"\"}." | wc -l`
		LANGUAGE=`grep "Language:" $i.translate | sed 's/% Language: //g'`
		LASTAUTH=`grep "Author:" $i.translate | head -n 1 | sed 's/% Author: //g'`
		echo -e "$i\t$MISSING\t$LANGUAGE\t$LASTAUTH"
	done

	cd $MSGS_DIR
	REVISION=`svn info | grep "^Rev" | head -1 | awk '{print $2}'`
	zip $HOME/ejabberd-langs-$REVISION.zip *.translate;

	rm *.translate
}

find_unused_full ()
{
	DATFILE=$1
	DATFILEI=$1.old_cleaned
	DELFILE=$2
	cd msgs

	DATFILE1=$DATFILE.t1
	DATFILE2=$DATFILE.t2

	DELFILE1=$DELFILE.t1
	DELFILE2=$DELFILE.t2
	DELFILEF=$DATFILE.unused.full
	echo "" >$DELFILEF

	grep -v "\\\\" $DELFILE >$DELFILE2
	echo ENDFILEMARK >>$DELFILE2
	cp $DATFILE $DATFILEI
	cp $DATFILE $DATFILE2

	cp $DELFILE2 $DELFILE1
	STRING=`head -1 $DELFILE1`
	until [[ $STRING == ENDFILEMARK ]]; do 
		cp $DELFILE2 $DELFILE1
		cp $DATFILE2 $DATFILE1

		STRING=`head -1 $DELFILE1`

		cat $DATFILE1 | grep "$STRING" >>$DELFILEF
		cat $DATFILE1 | grep -v "$STRING" >$DATFILE2
		cat $DELFILE1 | grep -v "$STRING" >$DELFILE2
	done

	mv $DATFILE2 $DATFILEI

	rm -f $MSGS_PATH.t1
	rm $MSGS_PATH.unused
	rm -f $MSGS_PATH.unused.t1
	rm $MSGS_PATH.unused.t2

	cd ..
}

translation_instructions ()
{
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
}

case "$1" in
	-help)
		echo "Options:"
		echo "  -langall"
		echo "  -lang LANGUAGE_FILE"
		echo ""
		echo "Example:"
		echo "  ./prepare-translation.sh -lang es.msg"
		exit 0
		;;
	-lang)
		LANGU=$2
		prepare_dirs
		extract_lang $LANGU
		shift
		shift
		;;
	-langall)
		prepare_dirs
		extract_lang_all
		shift
		;;
	*)
		echo "unknown option: '$1 $2'"
		shift
		shift
		;;
esac

echo ""
echo "End."
