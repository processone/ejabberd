#!/bin/bash

# Frontend for ejabberd's extract-tr.sh

# How to create template files for a new language:
# NEWLANG=zh
# cp priv/msgs/ejabberd.pot priv/msgs/$NEWLANG.po
# echo \{\"\",\"\"\}. > priv/msgs/$NEWLANG.msg
# make translations

extract_lang_src2pot ()
{
	./tools/extract-tr.sh src $DEPS_DIR/xmpp/src > $PO_DIR/ejabberd.pot
  ./tools/extract-erlydtl-templates.sh "priv/mod_invites/*.*" $PO_DIR/templates.pot
  msgcat $PO_DIR/ejabberd.pot $PO_DIR/templates.pot > $PO_DIR/temp.pot
  mv $PO_DIR/temp.pot $PO_DIR/ejabberd.pot
}

extract_lang_popot2po ()
{
	LANG_CODE=$1
	PO_PATH=$PO_DIR/$LANG_CODE.po
	POT_PATH=$PO_DIR/$PROJECT.pot

	msgmerge $PO_PATH $POT_PATH >$PO_PATH.translate 2>>$LOG
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

	cd $PO_DIR || exit

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
	{
	    echo "%% Generated automatically"
	    echo "%% DO NOT EDIT: run \`make translations\` instead"
	    echo "%% To improve translations please read:"
	    echo "%%   https://docs.ejabberd.im/developer/extending-ejabberd/localization/"
	    echo ""
	} >>$MSGS_PATH
	paste -d , $MSGID_PATH $MSGSTR_PATH | awk '{print "{" $0 "}."}' | sort -g >>$MSGS_PATH

	rm $MS_PATH
	rm $MSGID_PATH
	rm $MSGSTR_PATH

	mv $MSGS_PATH $MSGS_DIR
}

extract_lang_updateall ()
{
	echo ""
	echo "Generating POT..."
	extract_lang_src2pot

	cd $MSGS_DIR || exit
	echo ""
	echo "File Missing (fuzzy) Language     Last translator"
	echo "---- ------- ------- --------     ---------------"
	for i in *.msg ; do
                LANG_CODE=${i%.msg}
		printf "%s" "$LANG_CODE" | awk '{printf "%-6s", $1 }'

		PO=$PO_DIR/$LANG_CODE.po

		extract_lang_popot2po $LANG_CODE
		extract_lang_po2msg $LANG_CODE

                MISSING=$(msgfmt --statistics $PO 2>&1 | awk '{printf "%5s", $4+$7 }')
		printf " %s" "$MISSING"

                FUZZY=$(msgfmt --statistics $PO 2>&1 | awk '{printf "%7s", $4 }')
		printf " %s" "$FUZZY"

                LANGUAGE=$(grep "X-Language:" $PO | sed 's/\"X-Language: //g' | sed 's/\\n\"//g' | awk '{printf "%-12s", $1}')
		printf " %s" "$LANGUAGE"

                LASTAUTH=$(grep "Last-Translator" $PO | sed 's/\"Last-Translator: //g' | sed 's/\\n\"//g')
		echo " $LASTAUTH"
	done
	echo ""
	rm messages.mo
	grep -v " done" $LOG
	rm $LOG

	cd ..
}

EJA_DIR=$(pwd)
PROJECT=ejabberd
DEPS_DIR=$1
MSGS_DIR=$EJA_DIR/priv/msgs
LOG=/tmp/ejabberd-translate-errors.log
PO_DIR=$EJA_DIR/$DEPS_DIR/ejabberd_po/src/
if [ ! -f $EJA_DIR/$DEPS_DIR/ejabberd_po/src/ejabberd.pot ]; then
    echo "Couldn't find the required ejabberd_po repository in"
    echo "  $PO_DIR"
    echo "Run: ./configure --enable-tools; ./rebar get-deps"
    exit 1
fi
echo "Using PO files from $PO_DIR."

extract_lang_updateall
