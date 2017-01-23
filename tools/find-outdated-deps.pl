#!/usr/bin/perl


### Remmber to call `rebar update-deps` before calling this script

use strict;
use File::Slurp qw(slurp);
use feature qw(say);

sub cmp_ver {
    my @a = split /(\d+)/, $a;
    my @b = split /(\d+)/, $b;
    my $is_num = 1;

    while (1) {
        my $ap = shift @a;
        my $bp = shift @b;
        $is_num = 1 - $is_num;

        if (defined $ap) {
            if (defined $bp) {
                if ($is_num) {
                    next if $ap == $bp;
                    return 1 if $ap > $bp;
                    return -1;
                } else {
                    next if $ap eq $bp;
                    return 1 if $ap gt $bp;
                    return -1;
                }
            } else {
                return 1;
            }
        } elsif (defined $bp) {
            return -1;
        } else {
            return 0;
        }
    }
}

sub get_deps {
    my ($dir) = @_;
    my (%deps, %floating_deps);

    return (\%deps, \%floating_deps) unless -f "$dir/rebar.config";

    my $config = slurp("$dir/rebar.config");

    while ($config =~ /{\s*([^{]*?)\s*,[^{]*?{\s*git\s*[^{]*?{\s*tag\s*,\s*"(.*?)"\s*}/gs) {
        $deps{$1} = $2;
    }

    if ($config =~ /{floating_deps\s*,\s*\[\s*([^\]]+)\s*\]}/s) {
        my $fd = $1;
        while ($fd =~ /(\w+)/g) {
            $floating_deps{$1} = 1;
        }
    }
    return (\%deps, \%floating_deps);
}

my $base_dir = ".";

while (not -f "$base_dir/rebar.config") {
    $base_dir = "../$base_dir";
}

my (%deps, %floating_deps);
{
    my ($deps, $floating_deps) = get_deps($base_dir);
    %deps = %$deps;
    %floating_deps = %$floating_deps
}

my $all = $ARGV[0] eq "--all";

my %cross_deps;

$cross_deps{$_}->{$deps{$_}} = ["ejabberd"] for keys %deps;

for (sort keys %deps) {
    my $dep = $_;
    my $dep_dir = "$base_dir/deps/$dep";
    next unless -d $dep_dir;
    next unless $all or exists $floating_deps{$dep};

    my @tags0 = `git -C $dep_dir tag`;
    chomp(@tags0);
    my @tags = grep { /^v?\d+(?:\.\d+)*$/ } @tags0;
    @tags = sort cmp_ver @tags;

    #say "versions: ", join ", ", @tags;

    my $last_tag = $tags[$#tags];
    my @new = `git -C $dep_dir log --oneline $last_tag..origin/master`;
    chomp(@new);

    my ($cdeps, $fdeps) = get_deps($dep_dir);

    for (keys %$cdeps) {
        push @{$cross_deps{$_}->{$cdeps->{$_}}}, $dep;
    }

    if ($deps{$dep} ne $last_tag) {
        if (@new) {
            say $dep," can be updated: ", $deps{$dep}, " -> ", $last_tag, " and has ",
            scalar(@new), " commits after last tag:";
            say "   $_" for @new;
        } else {
            say $dep," can be updated: ", $deps{$dep}, " -> ", $last_tag;
        }
    } elsif (@new) {
        say $dep, " has ", scalar(@new), " commits after last tag:";
        say "   $_" for @new;
    }
}

say "";
for (sort keys %cross_deps) {
    my $dep = $_;

    if ((scalar keys %{$cross_deps{$dep}}) > 1) {
        say "Dependency $dep is requested in different versions:";
        say "   $_ -> ", join(", ", @{$cross_deps{$dep}->{$_}}) for sort cmp_ver keys %{$cross_deps{$dep}};
    }
}
