#!/usr/bin/env perl

# Copyright (c) 2012 Prashant Sharma.
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Supported by Prashant Sharma <scrapcodes@gmail.com>

# A script which is use in association with mod_ext_offline_msg_hook for handling 
# an offline message with out messing with the erlang code by using this 
# (Or anything on similar lines) example pluggable perl script. 
# Typical use cases: Sending a email notification for offline pings
# (you might wanna use sendmail). 
# OR 
# save to disk(thats what we demonstrate here).  
# Please refer to example.config file for usage.


use strict;
use warnings;

sub offline_message_hook($$$) {
	my ($from, $to, $packet) = @_;
	my $response;
	open (MYFILE, '>>~/data.txt');
	print MYFILE ":\n".$packet.$from.$to;
	close (MYFILE);
	return 1;
}
 
sub permit($$$) {
    my ($user, $domain, $operation) = @_;
    syswrite(STDOUT, pack('nn', 2, 1));
}

sub deny($$$) {
    my ($user, $domain, $operation) = @_;
    syswrite(STDOUT, pack('nn', 2, 0));
}

sub report($$@) {
	my ($level, $format, @args) = @_;

	syslog($level, "[%s] $format", uc($level), @args);

	if ($debug) {
		$format =~ s/%m/$!/;
		syswrite(STDERR, sprintf("[%s] $format\n", uc($level), @args));
	}
}


while (1) {
    my $buf;
	
    if (sysread(STDIN, $buf, 2) != 2) {
        report('info', 'The ejabberd port has been closed, exiting.');
        closelog();
        exit(0);
    }
    my $len = unpack('n', $buf);

    if (sysread(STDIN, $buf, $len) != $len) {
        report('crit', 'Reading %u bytes from ejabberd failed, exiting.', $len);
        closelog();
        exit(1);
    }
    my ($op, $user, $domain, $password) = split(/:/, $buf, 4);

    if (not defined($domain) or (not defined($password)
        and $op =~ /^(?:offline|someothernewfunctionsupport)$/)) {
        report('crit', 'Cannot parse ejabberd request, exiting.');
        closelog();
        exit(1);
    }

    if ($op eq 'offline') {
        if(offline_message_hook($user, $domain, $password)) {
            permit($user, $domain, '');
        } else {
            deny($user, $domain, '');
        }
    }
}
