#!/usr/bin/perl -w

use strict; # har har

use constant ERR => 0;
use constant WARN => 1;
use constant INFO => 2;
use constant DEBUG => 3;

use constant RID => 31974;

### ### ### conf ### ### ###

my $BASE_ADDRESS = "http://localhost:5280/http-bind/";
my $JABBER_SERVER = "localhost";

my $RID = RID;

my $WAIT = 60;

my $USER = "tester";
my $UPW = "mysecret";

my $DEBUG = INFO;

### ### ### END conf ### ### ###

# create an agent we can use for our requests
use LWP::UserAgent;
my $ua = new LWP::UserAgent();

# create a tree parser to parse response content
use XML::Parser;
my $p = new XML::Parser(Style => 'Tree');


### ### ### subs ### ### ###
sub doSend() {
	my $content = shift;
	
	# create a request
	my $req = new HTTP::Request(POST => $BASE_ADDRESS);
	$req->content_type('text/xml; charset=utf-8');
	$req->content($content);

	debug(DEBUG,"<< Request\n".$req->as_string."<< END Request");

	# send request
	my $res = $ua->request($req);

	debug(DEBUG,">> Response\n" . $res->as_string .">> END Response");
	return $res;
}

# getChildEls
# used to strip enclosing body element
# PARAMS: @tree - tree style array from XML::Parser
# RETURN: @children - child elements of top level element
sub getChildEls 
{
    my $t = $_[0];

    shift @{$t->[1]};
    return @{$t->[1]};
}

sub debug
{
    my $lvl = shift;
    my $msg = shift;

    return if ($DEBUG < $lvl);

    my $prefix = "[";
    $prefix .= "ERROR" if ($lvl == ERR);
    $prefix .= "WARNING" if ($lvl == WARN);
    $prefix .= "INFO" if ($lvl == INFO);
    $prefix .= "DEBUG" if ($lvl == DEBUG);
    $prefix .= "] ";

    $msg =~ s/\n/\n$prefix/g;
    print STDERR $prefix . $msg . "\n";
}
### ### ### main ### ### ###

$| = 1; # set streaming output

# no body
print "Sending some 'foo': ";
my $res = &doSend("foo");
if ($res->code == 400) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}
# no body
print "Sending some '<foo />': ";
$res = &doSend("<foo />");
if ($res->code == 400) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}

# empty body
print "Sending empty body: ";
$res = &doSend("<body />");
if ($res->code == 400) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}

# fake a sid
print "Sending wrong sid: ";
$res = &doSend("<body sid='08154711' rid='$RID' xmlns='http://jabber.org/protocol/httpbind'/>");
if ($res->code == 404) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}

# forget to send 'to'
print "Missing 'to' attribute at session creation request: ";
$res = &doSend("<body content='text/xml; charset=utf-8' hold='0' rid='$RID' wait='60' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>");
if ($res->is_success && $res->content =~ /<body .*type=["']terminate['"]/ && $res->content =~/<body .*condition=["']improper-addressing['"] /) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}

# sending empty 'to' attribute
print "Empty 'to' attribute at session creation request: ";
$res = &doSend("<body content='text/xml; charset=utf-8' hold='0' rid='$RID' to='' wait='60' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>");
if ($res->is_success && $res->content =~ /<body .*type=["']terminate['"]/ && $res->content =~/<body .*condition=["']improper-addressing['"] /) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}

# forget to send a rid
print "Missing 'rid' attribute at session creation request: ";
$res = &doSend("<body content='text/xml; charset=utf-8' hold='0' to='$JABBER_SERVER' wait='60' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>");
if ($res->code == 404) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}
# trying to connect to non-existent domain
print "Connecting to non-existent domain: ";
$res = &doSend("<body content='text/xml; charset=utf-8' hold='0' rid='$RID' to='foo.bar' wait='60' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>");
if ($res->is_success && $res->content =~ /<body .*type=["']terminate['"]/ && $res->content =~/<body .*condition=["']host-unknown['"] /) {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}

# trying to connect to non-existent jabber server
print "Connecting to non-existent jabber server: ";
$res = &doSend("<body content='text/xml; charset=utf-8' hold='0' rid='$RID' to='www.jabber.org' wait='60' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>");
if ($res->is_success && $res->content =~ /<body .*type=["']terminate['"]/ && ($res->content =~/<body .*condition=["']remote-connection-failed['"] / || $res->content =~/<body .*condition=["']host-unknown['"] /))  {
	print "OK.\n";
} else {
	print "Failed!\n";
	print $res->as_string, "\n";
}

# connection to foreign server
#print "Connecting to foreign jabber server: ";
#$res = &doSend("<body content='text/xml; charset=utf-8' hold='0' rid='$RID' to='jwchat.org' wait='60' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>");
#if ($res->is_success && $res->content =~ /<body .*type=["']terminate['"]/ && $res->content =~/<body .*condition=["']host-unknown['"] /) {
#	print "OK.\n";
#} else {
#	print "Failed!\n";
#	print $res->as_string, "\n";
#}

my %sess;
sub getSess
{
    $sess{rid} = RID; # a rid to start
    $res = &doSend("<body content='text/xml; charset=utf-8' hold='0' rid='$sess{rid}' to='$JABBER_SERVER' wait='$WAIT' xml:lang='en' xmlns='http://jabber.org/protocol/httpbind'/>");
    if ($res->is_success) {
	my $t = $p->parse($res->content);
	%sess = %{$t->[1]->[0]};
	$sess{rid} = RID; # a rid to start

	if (defined($sess{sid}) && $sess{sid} ne "" && defined($sess{wait}) && $sess{wait} ne '' && $sess{wait} <= $WAIT) {
	    debug(INFO,"sid: $sess{sid}");
	    debug(INFO, "authid: $sess{authid}") if (defined($sess{authid}));
	    debug(INFO, "wait: $sess{wait}");
	    debug(INFO, "inactivity: $sess{inactivity}") if (defined($sess{inactivity}));
	    debug(INFO, "polling: $sess{polling}") if (defined($sess{polling}));
	    debug(INFO, "requests: $sess{requests}") if (defined($sess{requests}));
	    debug(INFO, "accept: $sess{accept}") if (defined($sess{accept}));
	    debug(INFO, "charsets: $sess{charsets}") if (defined($sess{charsets}));

	    debug (WARN, "authid missing") unless (defined($sess{authid}) && $sess{authid} ne '');
	    debug (WARN, "server indicates polling mode") if (defined($sess{requests}) && $sess{requests} == 1);
	    return 1;
	} 

    }

    debug(ERR, "sid missing") unless (defined($sess{sid}) && $sess{sid} ne "");
    debug(ERR, "wait missing") unless (defined($sess{wait}) && $sess{wait} ne '');
    debug(ERR, "wait bigger then requested") unless (defined($sess{wait}) && $sess{wait} ne '' && $sess{wait} <= $WAIT);

    debug(DEBUG, $res->as_string);
    return 0;
}

# try to get a real sid
print "Create a new session: ";
if (&getSess()) {
    print "OK.\n";
} else {
    debug(ERR, "Aborting.");
    exit(1);
}

# checking wait attribute
print "Creating another session with smaller 'wait' then before: ";
$WAIT = $sess{wait} - 1;
if (&getSess()) {
    print "OK.\n";
} else {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);
}

sub doAuth
{
# query auth
    $sess{rid}++;
    $res = &doSend("<body rid='$sess{rid}' sid='$sess{sid}' xmlns='http://jabber.org/protocol/httpbind'><iq type='get' id='auth1'><query xmlns='jabber:iq:auth'><username>$USER</username></query></iq></body>");
    my @els = (&getChildEls($p->parse($res->content)));
    unless ($els[0] eq 'iq' && $els[1]->[0]->{'type'} eq 'result') {
	debug(ERR, $res->content);
	return 0;
    }
       
# send auth
    $sess{rid}++;
    $res = &doSend("<body rid='$sess{rid}' sid='$sess{sid}' xmlns='http://jabber.org/protocol/httpbind'><iq type='set' id='auth2'><query xmlns='jabber:iq:auth'><username>$USER</username><resource>test</resource><password>$UPW</password></query></iq></body>");
    @els = (&getChildEls($p->parse($res->content)));
    unless ($els[0] eq 'iq' && $els[1]->[0]->{'type'} eq 'result') {
	debug(ERR, $res->content);
	return 0;
    }

    return 1;
}

print "Authenticating: ";
if (&doAuth()) {
    print "OK.\n";
} else {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);    
}


sub doPoll
{
    $sess{rid}++;
    return &doSend("<body rid='$sess{rid}' sid='$sess{sid}' xmlns='http://jabber.org/protocol/httpbind'/>");
 }


print "Polling with wrong 'rid': ";
$sess{rid}--;
$res = &doPoll();
if ($res->code != 404) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
 #   exit(1);    
}
print "OK.\n";
print "Checking if session terminated: ";
$res = &doPoll();
if ($res->code != 404) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
  #  exit(1);    
}
print "OK.\n";

print "Create a new session: ";
if (&getSess()) {
    print "OK.\n";
} else {
    debug(ERR, "Aborting.");
    exit(1);
}

print "Authenticating: ";
if (&doAuth()) {
    print "OK.\n";
} else {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
   # exit(1);    
}

print "Polling too frequently: ";
$res = &doPoll();
if ($res->code != 200) {
    print "First poll failed unexpectedly!\n";
    debug(ERR, "Aborting.");
    #exit(1);    
}
$res = &doPoll();
if ($res->code != 403) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    #exit(1);    
}
print "OK.\n";

print "Checking if session terminated: ";
$res = &doPoll();
if ($res->code != 404) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    #exit(1);    
}
print "OK.\n";

print "Create a new session: ";
if (&getSess()) {
    print "OK.\n";
} else {
    debug(ERR, "Aborting.");
    exit(1);
}

print "Authenticating: ";
if (&doAuth()) {
    print "OK.\n";
} else {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);
}

print "Test if proper polling is allowed: ";
$res = &doPoll();
if ($res->code != 200) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);    
}
sleep $sess{polling};
$res = &doPoll();
if ($res->code != 200) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);    
}
print "OK.\n";

print "Waiting for session to timeout: ";
my $STEP=10;
for (my $i=0; $i<$sess{inactivity}; $i+=$STEP) {
    print ".";
    sleep $STEP;
}
sleep 1; # take another nap
$res = &doPoll();
if ($res->code != 404) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);    
}
print "OK.\n";

print "Create a new session: ";
if (&getSess()) {
    print "OK.\n";
} else {
    debug(ERR, "Aborting.");
    exit(1);
}

print "Authenticating: ";
if (&doAuth()) {
    print "OK.\n";
} else {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);
}


# [TODO]
# Check for
# * KEY Sequence Algorithm Compliance
# * Too Many Simultaneous Connections (probably hard to achieve for polling mode)
# * request custom content-type/-encoding

# get roster
print "getting roster\n";
$sess{rid}++;
$res = &doSend("<body rid='$sess{rid}' sid='$sess{sid}' xmlns='http://jabber.org/protocol/httpbind'><iq type='get'><query xmlns='jabber:iq:roster' /></iq></body>");
debug(INFO, $res->content);

# send presence
print "sending presence\n";
$sess{rid}++;
$res = &doSend("<body rid='$sess{rid}' sid='$sess{sid}' xmlns='http://jabber.org/protocol/httpbind'><presence /></body>");
debug(INFO, $res->content);

# sending bullshit
print "sending bullshit\n";
$sess{rid}++;
$res = &doSend("<body rid='$sess{rid}' sid='$sess{sid}' xmlns='http://jabber.org/protocol/httpbind'>sending bullshit</body>");
debug(INFO, $res->content);

# send presence
print "sending xa presence\n";
$sess{rid}++;
$res = &doSend("<body rid='$sess{rid}' sid='$sess{sid}' xmlns='http://jabber.org/protocol/httpbind'><presence><show>xa</show></presence></body>");
debug(INFO, $res->content);

# disconnect
sleep 3;
print "logout\n";
$sess{rid}++;
$res = &doSend("<body rid='$sess{rid}' sid='$sess{sid}' type='terminate' xmlns='http://jabber.org/protocol/httpbind'><presence type='unavailable'/></body>");
debug(INFO, $res->content);


print "Checking if session terminated: ";
$res = &doPoll();
if ($res->code != 404) {
    print "FAILED!\n";
    debug(ERR, "Aborting.");
    exit(1);    
}
print "OK.\n";
