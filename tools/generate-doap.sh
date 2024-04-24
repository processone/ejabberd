#!/bin/bash

# Erlang modules in ejabberd use a custom module attribute [1]
# named -protocol to define what XEPs and RFCs that module implements.
# General protocols are defined in ejabberd.erl
#
# The supported syntax is:
#   -protocol({rfc, RFC-NUMBER}).
#   -protocol({xep, XEP-NUMBER, XEP-VERSION}).
#   -protocol({xep, XEP-NUMBER, XEP-VERSION, EJABBERD-VERSION, STATUS, COMMENTS}).
# Where
#   RFC-NUMBER, XEP-NUMBER :: integer()
#   XEP-VERSION, EJABBERD-VERSION :: atom()
#   STATUS, COMMENTS :: string()
# For example:
#   -protocol({rfc, 5766}).
#   -protocol({xep, 111, '0.2'}).
#   -protocol({xep, 222, '1.2.0', '17.09', "", ""}).
#   -protocol({xep, 333, '1.11.2', '21.09', "complete", ""}).
#   -protocol({xep, 333, '0.2.0', '21.09', "partial", "Only client X is supported"}).
#
# [1] https://www.erlang.org/doc/reference_manual/modules.html#module-attributes

write_doap_head()
{
    cat >"$1" <<-'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns="http://usefulinc.com/ns/doap#"
         xmlns:xmpp="https://linkmauve.fr/ns/xmpp-doap#"
         xmlns:schema="https://schema.org/">
  <Project>
    <name>ejabberd</name>
    <shortdesc>XMPP Server with MQTT Broker and SIP Service</shortdesc>
    <description>Robust, Ubiquitous and Massively Scalable Messaging Platform (XMPP Server, MQTT Broker, SIP Service)</description>
    <created>2002-11-16</created>
    <os>BSD</os>
    <os>Linux</os>
    <os>macOS</os>
    <os>Windows</os>
    <programming-langauge>Erlang</programming-langauge>
    <programming-langauge>C</programming-langauge>
    <category rdf:resource="https://linkmauve.fr/ns/xmpp-doap#category-jabber"/>
    <category rdf:resource="https://linkmauve.fr/ns/xmpp-doap#category-server"/>
    <category rdf:resource="https://linkmauve.fr/ns/xmpp-doap#category-xmpp"/>

    <homepage rdf:resource="https://www.ejabberd.im"/>
    <download-page rdf:resource="https://www.process-one.net/en/ejabberd/downloads/"/>
    <download-mirror rdf:resource="https://github.com/processone/ejabberd/tags"/>
    <license rdf:resource="https://raw.githubusercontent.com/processone/ejabberd/master/COPYING"/>
    <schema:logo rdf:resource="https://docs.ejabberd.im/assets/img/footer_logo_e@2x.png"/>
    <bug-database rdf:resource="https://github.com/processone/ejabberd/issues"/>
    <support-forum rdf:resource="xmpp:ejabberd@conference.process-one.net?join"/>
    <repository>
      <GitRepository>
        <location rdf:resource="https://github.com/processone/ejabberd.git"/>
        <browse rdf:resource="https://github.com/processone/ejabberd"/>
      </GitRepository>
    </repository>

EOF
}

write_doap_tail()
{
    cat >>"$1" <<-'EOF'
  </Project>
</rdf:RDF>
EOF
}

write_rfcs()
{
    rfc=rfc$1
    out=$2
    int=$(echo $1 | sed 's/^0*//')

    imp=$(grep "\-protocol({rfc, $int," $BASE/src/* | sed "s/.*src\/\(.*\).erl.*'\([0-9.-]*\)'.*/\1 \2/")
    [ "$imp" = "" ] && imp="NA 0.0"

    echo "    <implements rdf:resource=\"https://www.rfc-editor.org/info/$rfc\"/>" >>$out
}

write_xeps()
{
    xep=xep-$1
    out=$2
    comments2=""
    int=$(echo $1 | sed 's/^0*//')
    imp=$(grep "\-protocol({xep, $int," $BASE/src/* | sed "s/.*src\/\(.*\).erl.*'\([0-9.-]*\)'.*/\1 \2/")
    [ "$imp" = "" ] && imp="NA 0.0"

    sourcefiles=$(grep "\-protocol({xep, $int," $BASE/src/* | sed "s/.*src\/\(.*\).erl.*'\([0-9.-]*\)'.*/\1/" | tr '\012' ',' | sed 's|,$||' | sed 's|,|, |g' | sed 's|^ejabberd$||')

    versionsold=$(grep "\-protocol({xep, $int, .*'})\." $BASE/src/* | sed "s/.*'\([0-9.-]*\)'.*/\1/" | head -1)
    versionsnew=$(grep "\-protocol({xep, $int, .*\"})\." $BASE/src/* | sed "s/.*'\([0-9.-]*\)', '.*/\1/" | head -1)
    versions="$versionsold$versionsnew"

    since=$(grep "\-protocol({xep, $int, .*\"})\." $BASE/src/* | sed "s/.*', '\([0-9.-]*\)',.*/\1/" | head -1)
    status=$(grep "\-protocol({xep, $int, .*\"})\." $BASE/src/* | sed "s/.*', \"\([a-z]*\)\", \".*/\1/" | head -1)

    comments=$(grep "\-protocol({xep, $int, .*\"})\." $BASE/src/* | sed "s/.*\", \"\(.*\)\"}.*/\1/" | head -1)
    [ -n "$comments" ] && comments2=", $comments"
    note="$sourcefiles$comments2"

    {
     echo "    <implements>"
     echo "      <xmpp:SupportedXep>"
     echo "        <xmpp:xep rdf:resource=\"https://xmpp.org/extensions/$xep.html\"/>"
     echo "        <xmpp:version>$versions</xmpp:version>"
     echo "        <xmpp:since>$since</xmpp:since>"
     echo "        <xmpp:status>$status</xmpp:status>"
     echo "        <xmpp:note>$note</xmpp:note>"
     echo "      </xmpp:SupportedXep>"
     echo "    </implements>"
    } >>$out
}

[ $# -eq 1 ] && BASE="$1" || BASE="$PWD"
[ -d $BASE/doc ] || mkdir $BASE/doc
temp=tools/ejabberd.temp
final=ejabberd.doap

write_doap_head $final

grep "\-protocol({rfc" $BASE/src/* | sed "s/,//" | awk '{printf("%04d\n", $2)}' | sort -u | while IFS= read -r x_num
do
    write_rfcs $x_num $temp
done
echo "" >>$temp

grep "\-protocol({xep" $BASE/src/* | sed "s/,//" | awk '{printf("%04d\n", $2)}' | sort -u | while IFS= read -r x_num
do
    write_xeps $x_num $temp
done

cat $temp >>$final
rm $temp

write_doap_tail $final
