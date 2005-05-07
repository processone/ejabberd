# uni_parse.tcl --
#
#	This program parses the UnicodeData file and generates the
#	corresponding uni_data.c file with compressed character
#	data tables.  The input to this program should be rfc3454.txt
#
# Copyright (c) 1998-1999 by Scriptics Corporation.
# All rights reserved.
#
# Modified for ejabberd by Alexey Shchepin
# 
# RCS: @(#) $Id$


namespace eval uni {
    set shift 8;		# number of bits of data within a page
				# This value can be adjusted to find the
				# best split to minimize table size

    variable pMap;		# map from page to page index, each entry is
				# an index into the pages table, indexed by
				# page number
    variable pages;		# map from page index to page info, each
				# entry is a list of indices into the groups
				# table, the list is indexed by the offset
    variable groups;		# list of character info values, indexed by
				# group number, initialized with the
				# unassigned character group
}

proc uni::getValue {i} {
    variable casemap
    variable casemap2
    variable tablemap

    if {[info exists tablemap($i)]} {
	set tables $tablemap($i)
    } else {
	set tables {}
    }

    if {[info exists casemap2($i)]} {
	set multicase 1
	set delta $casemap2($i)
    } else {
	set multicase 0
	if {[info exists casemap($i)]} {
	    set delta $casemap($i)
	} else {
	    set delta 0
	}
    }

    if {abs($delta) > 0xFFFFF} {
	puts "delta must be less than 22 bits wide"
	exit
    }

    set ac 0
    set c11 0
    set c21 0
    set b1 0
    set d1 0
    set d2 0
    set xnp 0

    foreach tab $tables {
	switch -glob -- $tab {
	    C.1.1 {set c11 1}
	    C.2.1 {set c21 1}
	    C.*   {set ac 1}
	    A.1   {set ac 1}
	    B.1   {set b1 1}
	    D.1   {set d1 1}
	    D.2   {set d2 1}
	    XNP   {set xnp 1}
	}
    }

    set val [expr {($ac  << 0) |
		   ($c11 << 1) |
		   ($c21 << 2) |
		   ($b1  << 3) |
		   ($d1  << 4) |
		   ($d2  << 5) |
		   ($xnp << 6) |
		   ($multicase << 7) |
		   ($delta << 11)}]

    return $val
}

proc uni::getGroup {value} {
    variable groups

    set gIndex [lsearch -exact $groups $value]
    if {$gIndex == -1} {
	set gIndex [llength $groups]
	lappend groups $value
    }
    return $gIndex
}

proc uni::addPage {info} {
    variable pMap
    variable pages
    variable pages_map
    
    if {[info exists pages_map($info)]} {
	lappend pMap $pages_map($info)
    } else {
	set pIndex [llength $pages]
	lappend pages $info
	set pages_map($info) $pIndex
	lappend pMap $pIndex
    }
    return
}


proc uni::load_tables {data} {
    variable casemap
    variable casemap2
    variable multicasemap
    variable tablemap

    set multicasemap {}
    set table ""

    foreach line [split $data \n] {
	if {$table == ""} {
	    if {[regexp {   ----- Start Table (.*) -----} $line temp table]} {
		#puts "Start table '$table'"
	    }
	} else {
	    if {[regexp {   ----- End Table (.*) -----} $line temp table1]} {
		set table ""
	    } else {
		if {$table == "B.1"} {
		    if {[regexp {^   ([[:xdigit:]]+); ;} $line \
			     temp val]} {
			scan $val %x val
			if {$val <= 0x10ffff} {
			    lappend tablemap($val) $table
			}
		    }
		} elseif {$table == "B.2"} {
		    if {[regexp {^   ([[:xdigit:]]+); ([[:xdigit:]]+);} $line \
			     temp from to]} {
			scan $from %x from
			scan $to %x to
			if {$from <= 0x10ffff && $to <= 0x10ffff} {
			    set casemap($from) [expr {$to - $from}]
			}
		    } elseif {[regexp {^   ([[:xdigit:]]+); ([[:xdigit:]]+) ([[:xdigit:]]+);} $line \
			     temp from to1 to2]} {
			scan $from %x from
			scan $to1 %x to1
			scan $to2 %x to2
			if {$from <= 0x10ffff && \
				$to1 <= 0x10ffff && $to2 <= 0x10ffff} {
			    set casemap2($from) [llength $multicasemap]
			    lappend multicasemap [list $to1 $to2]
			}
		    } elseif {[regexp {^   ([[:xdigit:]]+); ([[:xdigit:]]+) ([[:xdigit:]]+) ([[:xdigit:]]+);} $line \
			     temp from to1 to2 to3]} {
			scan $from %x from
			scan $to1 %x to1
			scan $to2 %x to2
			scan $to3 %x to3
			if {$from <= 0x10ffff && \
				$to1 <= 0x10ffff && $to2 <= 0x10ffff && \
				$to3 <= 0x10ffff} {
			    set casemap2($from) [llength $multicasemap]
			    lappend multicasemap [list $to1 $to2 $to3]
			}
		    } else {
			#puts "missed: $line"
		    }
		    
		} elseif {$table != "B.3"} {
		    if {[regexp {^   ([[:xdigit:]]+)-([[:xdigit:]]+)} $line \
			     temp from to]} {
			scan $from %x from
			scan $to %x to
			for {set i $from} {$i <= $to && $i <= 0x10ffff} {incr i} {
			    lappend tablemap($i) $table
			}
		    } elseif {[regexp {^   ([[:xdigit:]]+)} $line \
			     temp val]} {
			scan $val %x val
			if {$val <= 0x10ffff} {
			    lappend tablemap($val) $table
			}
		    }
		}
	    }
	}
    }

    # XMPP nodeprep prohibited
    foreach val {22 26 27 2f 3a 3c 3e 40} {
	scan $val %x val
	lappend tablemap($val) XNP
    }
}

proc uni::buildTables {} {
    variable shift

    variable casemap
    variable tablemap

    variable pMap {}
    variable pages {}
    variable groups {}
    set info {}			;# temporary page info
    
    set mask [expr {(1 << $shift) - 1}]

    set next 0

    for {set i 0} {$i <= 0x10ffff} {incr i} {
	set gIndex [getGroup [getValue $i]]

	# Split character index into offset and page number
	set offset [expr {$i & $mask}]
	set page [expr {($i >> $shift)}]

	# Add the group index to the info for the current page
	lappend info $gIndex

	# If this is the last entry in the page, add the page
	if {$offset == $mask} {
	    addPage $info
	    set info {}
	}
    }
    return
}

proc uni::main {} {
    global argc argv0 argv
    variable pMap
    variable pages
    variable groups
    variable shift
    variable multicasemap

    if {$argc != 2} {
	puts stderr "\nusage: $argv0 <datafile> <outdir>\n"
	exit 1
    }
    set f [open [lindex $argv 0] r]
    set data [read $f]
    close $f

    load_tables $data
    buildTables
    puts "X = [llength $pMap]  Y= [llength $pages]  A= [llength $groups]"
    set size [expr {[llength $pMap] + [llength $pages]*(1<<$shift)}]
    puts "shift = $shift, space = $size"

    set f [open [file join [lindex $argv 1] uni_data.c] w]
    fconfigure $f -translation lf
    puts $f "/*
 * uni_data.c --
 *
 *	Declarations of Unicode character information tables.  This file is
 *	automatically generated by the uni_parse.tcl script.  Do not
 *	modify this file by hand.
 *
 * Copyright (c) 1998 by Scriptics Corporation.
 * All rights reserved.
 *
 * Modified for ejabberd by Alexey Shchepin
 *
 * RCS: @(#) \$Id\$
 */

/*
 * A 16-bit Unicode character is split into two parts in order to index
 * into the following tables.  The lower OFFSET_BITS comprise an offset
 * into a page of characters.  The upper bits comprise the page number.
 */

#define OFFSET_BITS $shift

/*
 * The pageMap is indexed by page number and returns an alternate page number
 * that identifies a unique page of characters.  Many Unicode characters map
 * to the same alternate page number.
 */

static unsigned char pageMap\[\] = {"
    set line "    "
    set last [expr {[llength $pMap] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	append line [lindex $pMap $i]
	if {$i != $last} {
	    append line ", "
	}
	if {[string length $line] > 70} {
	    puts $f $line
	    set line "    "
	}
    }
    puts $f $line
    puts $f "};

/*
 * The groupMap is indexed by combining the alternate page number with
 * the page offset and returns a group number that identifies a unique
 * set of character attributes.
 */

static unsigned short int groupMap\[\] = {"
    set line "    "
    set lasti [expr {[llength $pages] - 1}]
    for {set i 0} {$i <= $lasti} {incr i} {
	set page [lindex $pages $i]
	set lastj [expr {[llength $page] - 1}]
	for {set j 0} {$j <= $lastj} {incr j} {
	    append line [lindex $page $j]
	    if {$j != $lastj || $i != $lasti} {
		append line ", "
	    }
	    if {[string length $line] > 70} {
		puts $f $line
		set line "    "
	    }
	}
    }
    puts $f $line
    puts $f "};

/*
 * Each group represents a unique set of character attributes.  The attributes
 * are encoded into a 32-bit value as follows:
 *
 * Bit  0	A.1 | C.1.2 | C.2.2 | C.3 -- C.9
 *
 * Bit  1	C.1.1
 *
 * Bit  2	C.2.1
 *
 * Bit  3	B.1
 *
 * Bit  4	D.1
 *
 * Bit  5	D.2
 *
 * Bit  6	XNP
 *
 * Bit  7	Case maps to several characters
 *
 * Bits 8-10	Reserved for future use.
 *
 * Bits 11-31	Case delta: delta for case conversions.  This should be the
 *			    highest field so we can easily sign extend.
 */

static int groups\[\] = {"
    set line "    "
    set last [expr {[llength $groups] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	set val [lindex $groups $i]

	append line [format "%d" $val]
	if {$i != $last} {
	    append line ", "
	}
	if {[string length $line] > 65} {
	    puts $f $line
	    set line "    "
	}
    }
    puts $f $line
    puts $f "};

/*
 * Table for characters that lowercased to multiple ones
 */

static int multiCaseTable\[\]\[4\] = {"
    set last [expr {[llength $multicasemap] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	set val [lindex $multicasemap $i]

	set line "    "
	append line [format "{%d, %s}" [llength $val] [join $val ", "]]
	if {$i != $last} {
	    append line ", "
	}
	puts $f $line
    }
    puts $f "};

/*
 * The following constants are used to determine the category of a
 * Unicode character.
 */

#define ACMask  (1 << 0)
#define C11Mask (1 << 1)
#define C21Mask (1 << 2)
#define B1Mask  (1 << 3)
#define D1Mask  (1 << 4)
#define D2Mask  (1 << 5)
#define XNPMask (1 << 6)
#define MCMask  (1 << 7)

/*
 * The following macros extract the fields of the character info.  The
 * GetDelta() macro is complicated because we can't rely on the C compiler
 * to do sign extension on right shifts.
 */

#define GetCaseType(info) (((info) & 0xE0) >> 5)
#define GetCategory(info) ((info) & 0x1F)
#define GetDelta(info) (((info) > 0) ? ((info) >> 11) : (~(~((info)) >> 11)))
#define GetMC(info) (multiCaseTable\[GetDelta(info)\])

/*
 * This macro extracts the information about a character from the
 * Unicode character tables.
 */

#define GetUniCharInfo(ch) (groups\[groupMap\[(pageMap\[(((int)(ch)) & 0x1fffff) >> OFFSET_BITS\] << OFFSET_BITS) | ((ch) & ((1 << OFFSET_BITS)-1))\]\])
"

    close $f
}

uni::main

return
