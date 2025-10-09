#!/usr/bin/perl -p0

use strict;

sub sp{
    my $s=trim(shift);
    my @r;
    push @r, trim($1) while $s =~ m/
      (
        (?:
           [^(),]+ |
           (?: \( \) ) |
           \(
              ( [^()]+ | \( (?2) \) )+
           \)
         )+
      )
     /gx;
     return join(",\n",sort(@r))
 }

 sub trim {
     my $s = shift;
     $s =~ s/^\s+|\s+$//g;
     return $s;
 }

 s/--.*\n//gm;

 s/
    (
       [^()]+ |
       (?: \( \) )
    ) |
    \(
       (
          (?: (?R) | [^()]+ )+
       )
    \)/ if ($2) {
           "(\n".sp($2)."\n)"
         } else {
           $1
         }/gsxe;
