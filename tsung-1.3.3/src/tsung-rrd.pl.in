#!/usr/bin/perl
#
# Copyright (C) 2009 Bearstech  http://www.bearstech.com/
# Copyright (C) 2009 Rodolphe Quiédeville
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Auteur: Rodolphe Quiédeville (rodolphe@quiedeville.org)
# Version: $Id$

# purpose: create rrd files with tsung log datas

use strict;
use RRDs;
use Getopt::Long;

use vars qw ($help *verbose $version $log_file $output);

$log_file = "tsung.log";
$output = ".";

GetOptions( "help",\$help,
            "verbose",\$verbose,
            "log=s",\$log_file,
            "output=s",\$output,
            "version",\$version
          );

# check options
printf "dir %s doesn't exists\n",$output if (!-d $output);
printf "file %s doesn't exists\n",$log_file if (!-f $log_file);

# do the job
open (FILE, $log_file ) or die "Cannot open : $!";
while (<FILE>) {
    
    my $date = $1 if (/stats: dump at (\d+)/);
    
    users($date, $1, "users") if (/^stats: users (\d+) (\d+)$/);
    users($date, $1, "connected") if (/^stats: connected (\d+) (\d+)$/);
    
    codes($date, $1, $2) if (/^stats: (\d+) (\d+) (\d+)$/);
    
    generic($date, $1, $2) if (/^stats: (page) (\d+)/);
    generic($date, $1, $2) if (/^stats: (session) (\d+)/);

    generic($date, $1, $2) if (/^stats: (request) (\d+)/);
    generic($date, $1, $2) if (/^stats: (connect) (\d+)/);
    
    generic($date, $1, $2) if (/^stats: (size_\w+) (\d+)/);

    generic($date, $1, $2) if (/^stats: (users_count) (\d+)/);
    generic($date, $1, $2) if (/^stats: (finish_users_count) (\d+)/);
}
close FILE;

# users data
sub users {    
    my ($date,$value, $stat) = @_;

    RRDs::update ("users.rrd", "--template", $stat, "$date:$value");
}

#
# HTTP return code
#
sub codes {    
    my ($date, $code, $value) = @_;

    my $file = "$output/code-$code.rrd";

    create_rrd ($file) if (! -f $file);

    RRDs::update ($file, "--template", "value", "$date:$value");
}

#
# Some generic datas
#
sub generic {    
    my ($date, $data, $value) = @_;

    my $file = "$output/$data.rrd";

    create_rrd ($file) if (! -f $file);

    RRDs::update ($file, "--template", "value", "$date:$value");
}

#
# Create RRD file
#
sub create_rrd {

    my ($file) = @_;

    printf "Create %s\n",$file if $verbose;

    my @parms = ("$file", 
		 "--start",1240488000, 
		 "--step", 10,
		 "DS:value:GAUGE:20:0:671744",
		 "RRA:AVERAGE:0.5:1:2400",
		 "RRA:AVERAGE:0.5:2:1200");
    
    RRDs::create (@parms);
    
    if (my $ERROR = RRDs::error) {
	die "RRDs ERROR: $ERROR\n";
    };
}
