#!/bin/bash

# Frontend for ejabberd's extract_translations.erl
# by Badlop

# How to create template files for a new language:
# NEWLANG=zh
# cp msgs/ejabberd.pot msgs/$NEWLANG.po
# echo \{\"\",\"\"\}. > msgs/$NEWLANG.msg
# ../../extract_translations/prepare-translation.sh -updateall

prepare_dirs ()
{
	# Where is Erlang binary
	ERL=`which erl`

	EJA_SRC_DIR=$EJA_DIR/src/
	EJA_MSGS_DIR=$EJA_DIR/priv/msgs/
	EXTRACT_DIR=$EJA_DIR/contrib/extract_translations/
	EXTRACT_ERL=$EXTRACT_DIR/extract_translations.erl
	EXTRACT_BEAM=$EXTRACT_DIR/extract_translations.beam

	SRC_DIR=$RUN_DIR/src
	EBIN_DIR=$RUN_DIR/ebin
	MSGS_DIR=$EJA_DIR/priv/msgs

	if !([[ -n $EJA_DIR ]])
	then 
	echo "ejabberd dir does not exist: $EJA_DIR"
	fi

	if !([[ -x $EXTRACT_BEAM ]])
	then 
	sh -c "cd $EXTRACT_DIR; $ERL -compile $EXTRACT_ERL"
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
	for i in $( ls *.msg ) ; do
		extract_lang $i;
	done

	echo -e "File\tMissing\tLanguage\t\tLast translator"
	echo -e "----\t-------\t--------\t\t---------------"
	cd $MSGS_DIR
	for i in $( ls *.msg ) ; do
		MISSING=`cat $i.translate | grep "\", \"\"}." | wc -l`
		LANGUAGE=`grep "X-Language:" $i.translate | sed 's/% Language: //g'`
		LASTAUTH=`grep "Author:" $i.translate | head -n 1 | sed 's/% Author: //g'`
		echo -e "$i\t$MISSING\t$LANGUAGE\t$LASTAUTH"
	done

	cd $MSGS_DIR
	REVISION=`git describe --always`
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

extract_lang_srcmsg2po ()
{
	LANG=$1
	LANG_CODE=$LANG.$PROJECT
	MSGS_PATH=$MSGS_DIR/$LANG_CODE.msg
	PO_PATH=$MSGS_DIR/$LANG_CODE.po

	echo $MSGS_PATH

	cd $SRC_DIR
	$ERL -pa $EXTRACT_DIR -pa $EBIN_DIR -pa $EJA_SRC_DIR -pa ../include -noinput -noshell -s extract_translations -s init stop -extra -srcmsg2po . $MSGS_PATH >$PO_PATH.1
	sed -e 's/ \[\]$/ \"\"/g;' $PO_PATH.1 > $PO_PATH.2
	msguniq --sort-by-file $PO_PATH.2 --output-file=$PO_PATH
	
	rm $PO_PATH.*
}

extract_lang_src2pot ()
{
	LANG_CODE=$PROJECT
	MSGS_PATH=$MSGS_DIR/$LANG_CODE.msg
	POT_PATH=$MSGS_DIR/$LANG_CODE.pot

	echo -n "" >$MSGS_PATH
	echo "% Language: Language Name" >>$MSGS_PATH
	echo "% Author: Translator name and contact method" >>$MSGS_PATH
	echo "" >>$MSGS_PATH

	cd $SRC_DIR
	$ERL -pa $EXTRACT_DIR -pa $EBIN_DIR -pa $EJA_SRC_DIR -pa ../include -noinput -noshell -s extract_translations -s init stop -extra -srcmsg2po . $MSGS_PATH >$POT_PATH.1
	sed -e 's/ \[\]$/ \"\"/g;' $POT_PATH.1 > $POT_PATH.2

	#msguniq --sort-by-file $POT_PATH.2 $EJA_MSGS_DIR --output-file=$POT_PATH
	msguniq --sort-by-file $POT_PATH.2 --output-file=$POT_PATH

	rm $POT_PATH.*
	rm $MSGS_PATH

	# If the project is a specific module, not the main ejabberd
	if [[ $PROJECT != ejabberd ]] ; then
	    # Remove from project.pot the strings that are already present in the general ejabberd
	    EJABBERD_MSG_FILE=$EJA_MSGS_DIR/es.po # This is just some file with translated strings
	    POT_PATH_TEMP=$POT_PATH.temp
	    msgattrib --set-obsolete --only-file=$EJABBERD_MSG_FILE -o $POT_PATH_TEMP $POT_PATH
	    mv $POT_PATH_TEMP $POT_PATH
	fi
}

extract_lang_popot2po ()
{
	LANG_CODE=$1
	PO_PATH=$MSGS_DIR/$LANG_CODE.po
	POT_PATH=$MSGS_DIR/$PROJECT.pot

	msgmerge $PO_PATH $POT_PATH >$PO_PATH.translate 2>/dev/null
	mv $PO_PATH.translate $PO_PATH 
}

extract_lang_po2msg ()
{
	LANG_CODE=$1
	PO_PATH=$LANG_CODE.po
	MS_PATH=$PO_PATH.ms
	MSGID_PATH=$PO_PATH.msgid
	MSGSTR_PATH=$PO_PATH.msgstr
	MSGS_PATH=$LANG_CODE.msg

	cd $MSGS_DIR

	# Check PO has correct ~
	# Let's convert to C format so we can use msgfmt
	PO_TEMP=$LANG_CODE.po.temp
	cat $PO_PATH | sed 's/%/perc/g' | sed 's/~/%/g' | sed 's/#:.*/#, c-format/g' >$PO_TEMP
	msgfmt $PO_TEMP --check-format
	result=$?
	rm $PO_TEMP
	if [ $result -ne 0 ] ; then
		exit 1
	fi

	msgattrib $PO_PATH --translated --no-fuzzy --no-obsolete --no-location --no-wrap | grep "^msg" | tail --lines=+3 >$MS_PATH
	grep "^msgid" $PO_PATH.ms | sed 's/^msgid //g' >$MSGID_PATH
	grep "^msgstr" $PO_PATH.ms | sed 's/^msgstr //g' >$MSGSTR_PATH
	echo "%% -*- coding: latin-1 -*-" >$MSGS_PATH
	paste $MSGID_PATH $MSGSTR_PATH --delimiter=, | awk '{print "{" $0 "}."}' | sort -g >>$MSGS_PATH

	rm $MS_PATH
	rm $MSGID_PATH
	rm $MSGSTR_PATH
}

extract_lang_updateall ()
{
	echo "Generating POT"
	extract_lang_src2pot 

	cd $MSGS_DIR
	echo ""
	echo -e "File Missing Language     Last translator"
	echo -e "---- ------- --------     ---------------"
	for i in $( ls *.msg ) ; do
                LANG_CODE=${i%.msg}
		echo -n $LANG_CODE | awk '{printf "%-6s", $1 }'

		# Convert old MSG file to PO
		PO=$LANG_CODE.po
		[ -f $PO ] || extract_lang_srcmsg2po $LANG_CODE

		extract_lang_popot2po $LANG_CODE
		extract_lang_po2msg $LANG_CODE

		MISSING=`msgfmt --statistics $PO 2>&1 | awk '{printf "%5s", $4 }'`
		echo -n " $MISSING"

		LANGUAGE=`grep "X-Language:" $PO | sed 's/\"X-Language: //g' | sed 's/\\\\n\"//g' | awk '{printf "%-12s", $1}'`
		echo -n " $LANGUAGE"

		LASTAUTH=`grep "Last-Translator" $PO | sed 's/\"Last-Translator: //g' | sed 's/\\\\n\"//g'`
		echo " $LASTAUTH"
	done
	echo ""
	rm messages.mo

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

EJA_DIR=`pwd`
RUN_DIR=`pwd`
PROJECT=ejabberd

while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
        --) break ;;
	-project)
		PROJECT=$1
		shift
		;;
	-ejadir)
		EJA_DIR=$1
		shift
		;;
	-rundir)
		RUN_DIR=$1
		shift
		;;
	-lang)
		LANGU=$1
		prepare_dirs
		extract_lang $LANGU
		shift
		;;
	-langall)
		prepare_dirs
		extract_lang_all
		;;
	-srcmsg2po)
		LANG_CODE=$1
		prepare_dirs
		extract_lang_srcmsg2po $LANG_CODE
		shift
		;;
	-popot2po)
		LANG_CODE=$1
		prepare_dirs
		extract_lang_popot2po $LANG_CODE
		shift
		;;
	-src2pot)
		prepare_dirs
		extract_lang_src2pot 
		;;
	-po2msg)
		LANG_CODE=$1
		prepare_dirs
		extract_lang_po2msg $LANG_CODE
		shift
		;;
	-updateall)
		prepare_dirs
		extract_lang_updateall
		;;
	*)
		echo "Options:"
		echo "  -langall"
		echo "  -lang LANGUAGE_FILE"
		echo "  -srcmsg2po LANGUAGE   Construct .msg file using source code to PO file"
		echo "  -src2pot              Generate template POT file from source code"
		echo "  -popot2po LANGUAGE    Update PO file with template POT file"
		echo "  -po2msg LANGUAGE      Export PO file to MSG file"
		echo "  -updateall            Generate POT and update all PO"
		echo ""
		echo "Example:"
		echo "  ./prepare-translation.sh -lang es.msg"
		exit 0
		;;
    esac
done
