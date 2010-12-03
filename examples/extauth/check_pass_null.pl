#!/usr/bin/perl

use Unix::Syslog qw(:macros :subs);

my $domain = $ARGV[0] || "example.com";

while(1)
  {
   # my $rin = '',$rout;
   # vec($rin,fileno(STDIN),1) = 1;
   # $ein = $rin;
   # my $nfound = select($rout=$rin,undef,undef,undef);

    my $buf = "";
    syslog LOG_INFO,"waiting for packet";
    my $nread = sysread STDIN,$buf,2;
    do { syslog LOG_INFO,"port closed"; exit; } unless $nread == 2;
    my $len = unpack "n",$buf;
    my $nread = sysread STDIN,$buf,$len;

    my ($op,$user,$host,$password) = split /:/,$buf;
    #$user =~ s/\./\//og;
    my $jid = "$user\@$domain";
    my $result;

    syslog(LOG_INFO,"request (%s)", $op);

  SWITCH:
      {
	$op eq 'auth' and do
	  {
             $result = 1;
	  },last SWITCH;

	$op eq 'setpass' and do
	  {
             $result = 1;
	  },last SWITCH;

        $op eq 'isuser' and do
          {
             # password is null. Return 1 if the user $user\@$domain exitst.
             $result = 1;
          },last SWITCH;

        $op eq 'tryregister' and do
          {
             $result = 1;
          },last SWITCH;

        $op eq 'removeuser' and do
          {
             # password is null. Return 1 if the user $user\@$domain exitst.
             $result = 1;
          },last SWITCH;

        $op eq 'removeuser3' and do
          {
             $result = 1;
          },last SWITCH;
      };
    my $out = pack "nn",2,$result ? 1 : 0;
    syswrite STDOUT,$out;
  }

closelog;
