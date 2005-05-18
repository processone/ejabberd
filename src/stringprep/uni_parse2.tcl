# uni_parse2.tcl --
#
#	This program parses the UnicodeData file and generates the
#	corresponding uni_norm.c file with compressed character
#	data tables.  The input to this program should be
#	UnicodeData-3.2.0.txt and CompositionExclusions-3.2.0.txt files from:
#	    ftp://ftp.unicode.org/Public/UNIDATA/
#
# Copyright (c) 1998-1999 by Scriptics Corporation.
# All rights reserved.
#
# Modified for ejabberd by Alexey Shchepin
# 
# RCS: @(#) $Id$


namespace eval uni {
    set cclass_shift 8
    set decomp_shift 8
    set comp_shift 8
    set shift 5;		# number of bits of data within a page
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

    variable categories {
	Cn Lu Ll Lt Lm Lo Mn Me Mc Nd Nl No Zs Zl Zp
	Cc Cf Co Cs Pc Pd Ps Pe Pi Pf Po Sm Sc Sk So
    };				# Ordered list of character categories, must
				# match the enumeration in the header file.

    variable titleCount 0;	# Count of the number of title case
				# characters.  This value is used in the
				# regular expression code to allocate enough
				# space for the title case variants.
}

proc uni::getValue {items index} {
    variable categories
    variable titleCount

    # Extract character info

    set category [lindex $items 2]
    if {[scan [lindex $items 12] %4x toupper] == 1} {
	set toupper [expr {$index - $toupper}]
    } else {
	set toupper {}
    }
    if {[scan [lindex $items 13] %4x tolower] == 1} {
	set tolower [expr {$tolower - $index}]
    } else {
	set tolower {}
    }
    if {[scan [lindex $items 14] %4x totitle] == 1} {
	set totitle [expr {$index - $totitle}]
    } else {
	set totitle {}
    }

    set categoryIndex [lsearch -exact $categories $category]
    if {$categoryIndex < 0} {
	puts "Unexpected character category: $index($category)"
	set categoryIndex 0
    } elseif {$category == "Lt"} {
	incr titleCount
    }

    return "$categoryIndex,$toupper,$tolower,$totitle"
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
    
    set pIndex [lsearch -exact $pages $info]
    if {$pIndex == -1} {
	set pIndex [llength $pages]
	lappend pages $info
    }
    lappend pMap $pIndex
    return
}

proc uni::addPage {map_var pages_var info} {
    variable $map_var
    variable $pages_var
    
    set pIndex [lsearch -exact [set $pages_var] $info]
    if {$pIndex == -1} {
	set pIndex [llength [set $pages_var]]
	lappend $pages_var $info
    }
    lappend $map_var $pIndex
    return
}

proc uni::load_exclusions {data} {
    variable exclusions

    foreach line [split $data \n] {
	if {$line == ""} continue

	set items [split $line " "]

	if {[lindex $items 0] == "#"} continue

	scan [lindex $items 0] %x index

	set exclusions($index) ""
    }
}

proc uni::load_tables {data} {
    variable cclass_map
    variable decomp_map
    variable comp_map
    variable comp_first
    variable comp_second
    variable exclusions

    foreach line [split $data \n] {
	if {$line == ""} continue

	set items [split $line \;]

	scan [lindex $items 0] %x index
	set cclass [lindex $items 3]
	set decomp [lindex $items 5]

	set cclass_map($index) $cclass
	#set decomp_map($index) $cclass

	if {$decomp != ""} {
	    if {[string index [lindex $decomp 0] 0] == "<"} {
		set decomp1 [lreplace $decomp 0 0]
		set decomp {}
		foreach ch $decomp1 {
		    scan $ch %x ch
		    lappend decomp $ch
		}
		set decomp_map($index) $decomp
	    } else {
		switch -- [llength $decomp] {
		    1 {
			scan $decomp %x ch
			set decomp_map($index) $ch
		    }
		    2 {
			scan $decomp "%x %x" ch1 ch2
			set decomp [list $ch1 $ch2]
			set decomp_map($index) $decomp
			# hackish
			if {(![info exists cclass_map($ch1)] || \
				 $cclass_map($ch1) == 0) && \
				![info exists exclusions($index)]} {
			    if {[info exists comp_first($ch1)]} {
				incr comp_first($ch1)
			    } else {
				set comp_first($ch1) 1
			    }
			    if {[info exists comp_second($ch2)]} {
				incr comp_second($ch2)
			    } else {
				set comp_second($ch2) 1
			    }
			    set comp_map($decomp) $index
			} else {
			    puts "Excluded $index"
			}
		    }
		    default {
			puts "Bad canonical decomposition: $line"
		    } 
		}
	    }

	    #puts "[format 0x%0.4x $index]\t$cclass\t$decomp_map($index)"
	}
    }
    #puts [array get comp_first]
    #puts [array get comp_second]
}

proc uni::buildTables {} {
    variable cclass_shift
    variable decomp_shift
    variable comp_shift

    variable cclass_map
    variable cclass_pmap {}
    variable cclass_pages {}
    variable decomp_map
    variable decomp_pmap {}
    variable decomp_pages {}
    variable decomp_list {}
    variable comp_map
    variable comp_pmap {}
    variable comp_pages {}
    variable comp_first
    variable comp_second
    variable comp_first_list {}
    variable comp_second_list {}
    variable comp_x_list {}
    variable comp_y_list {}
    variable comp_both_map {}

    set cclass_info {}
    set decomp_info {}
    set comp_info {}
    
    set cclass_mask [expr {(1 << $cclass_shift) - 1}]
    set decomp_mask [expr {(1 << $decomp_shift) - 1}]
    set comp_mask [expr {(1 << $comp_shift) - 1}]

    foreach comp [array names comp_map] {
	set ch1 [lindex $comp 0]
	if {[info exists comp_first($ch1)] && $comp_first($ch1) > 0 && \
		[info exists comp_second($ch1)] && $comp_second($ch1) > 0} {
	    if {[lsearch -exact $comp_x_list $ch1] < 0} {
		set i [llength $comp_x_list]
		lappend comp_x_list $ch1
		set comp_info_map($ch1) $i
		lappend comp_y_list $ch1
		set comp_info_map($ch1) $i
		puts "There should be no symbols which appears on"
		puts "both first and second place in composition"
		exit
	    }
	}
    }

    foreach comp [array names comp_map] {
	set ch1 [lindex $comp 0]
	set ch2 [lindex $comp 1]

	if {$comp_first($ch1) == 1 && ![info exists comp_second($ch1)]} {
	    set i [llength $comp_first_list]
	    lappend comp_first_list [list $ch2 $comp_map($comp)]
	    set comp_info_map($ch1) [expr {$i | (1 << 16)}]
	} elseif {$comp_second($ch2) == 1 && ![info exists comp_first($ch2)]} {
	    set i [llength $comp_second_list]
	    lappend comp_second_list [list $ch1 $comp_map($comp)]
	    set comp_info_map($ch2) [expr {$i | (1 << 16) | (1 << 17)}]
	} else {
	    if {[lsearch -exact $comp_x_list $ch1] < 0} {
		set i [llength $comp_x_list]
		lappend comp_x_list $ch1
		set comp_info_map($ch1) $i
	    }
	    if {[lsearch -exact $comp_y_list $ch2] < 0} {
		set i [llength $comp_y_list]
		lappend comp_y_list $ch2
		set comp_info_map($ch2) [expr {$i | (1 << 17)}]
	    }
	}
    }

    set next 0

    for {set i 0} {$i <= 0x10ffff} {incr i} {
	#set gIndex [getGroup [getValue $i]]

	set cclass_offset [expr {$i & $cclass_mask}]

	if {[info exists cclass_map($i)]} {
	    set cclass $cclass_map($i)
	} else {
	    set cclass 0
	}
	lappend cclass_info $cclass

	if {$cclass_offset == $cclass_mask} {
	    addPage cclass_pmap cclass_pages $cclass_info
	    set cclass_info {}
	}


	set decomp_offset [expr {$i & $decomp_mask}]

	if {[info exists decomp_map($i)]} {
	    set decomp $decomp_map($i)
	    set b 1
	    while {$b} {
		set b 0
		for {set j 0} {$j < [llength $decomp]} {incr j} {
		    if {[info exists \
			     decomp_map([set ch1 [lindex $decomp $j]])]} {
			#puts -$decomp
			set decomp [eval [list lreplace $decomp $j $j] \
					$decomp_map($ch1)]
			#puts +$decomp
			set b 1
		    }
		}
	    }

	    if {[info exists decomp_used($decomp)]} {
		lappend decomp_info $decomp_used($decomp)
	    } else {
		set val [expr {([llength $decomp] << 16) + \
				   [llength $decomp_list]}]
		#set val [expr {[llength $decomp_list]}]
		lappend decomp_info $val
		set decomp_used($decomp) $val
		#puts "$val $decomp"
		foreach d $decomp {
		    lappend decomp_list $d
		}
	    }
	} else {
	    lappend decomp_info -1
	}

	if {$decomp_offset == $decomp_mask} {
	    addPage decomp_pmap decomp_pages $decomp_info
	    set decomp_info {}
	}


	set comp_offset [expr {$i & $comp_mask}]

	if {[info exists comp_info_map($i)]} {
	    set comp $comp_info_map($i)
	} else {
	    set comp -1
	}
	lappend comp_info $comp

	if {$comp_offset == $comp_mask} {
	    addPage comp_pmap comp_pages $comp_info
	    set comp_info {}
	}
    }

    #puts [array get decomp_map]
    #puts $decomp_list

    return
}

proc uni::main {} {
    global argc argv0 argv
    variable cclass_shift
    variable cclass_pmap
    variable cclass_pages
    variable decomp_shift
    variable decomp_pmap
    variable decomp_pages
    variable decomp_list
    variable comp_shift
    variable comp_map
    variable comp_pmap
    variable comp_pages
    variable comp_first_list
    variable comp_second_list
    variable comp_x_list
    variable comp_y_list
    variable pages
    variable groups {}
    variable titleCount

    if {$argc != 3} {
	puts stderr "\nusage: $argv0 <datafile> <exclusionsfile> <outdir>\n"
	exit 1
    }
    set f [open [lindex $argv 1] r]
    set data [read $f]
    close $f

    load_exclusions $data

    set f [open [lindex $argv 0] r]
    set data [read $f]
    close $f

    load_tables $data
    buildTables
    #puts "X = [llength $pMap]  Y= [llength $pages]  A= [llength $groups]"
    #set size [expr {[llength $pMap] + [llength $pages]*(1<<$shift)}]
    #puts "shift = 6, space = $size"
    #puts "title case count = $titleCount"

    set f [open [file join [lindex $argv 2] uni_norm.c] w]
    fconfigure $f -translation lf
    puts $f "/*
 * uni_norm.c --
 *
 *	Declarations of Unicode character information tables.  This file is
 *	automatically generated by the uni_parse2.tcl script.  Do not
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
 * into the following tables.  The lower CCLASS_OFFSET_BITS comprise an offset
 * into a page of characters.  The upper bits comprise the page number.
 */

#define CCLASS_OFFSET_BITS $cclass_shift

/*
 * The pageMap is indexed by page number and returns an alternate page number
 * that identifies a unique page of characters.  Many Unicode characters map
 * to the same alternate page number.
 */

static unsigned char cclassPageMap\[\] = {"
    set line "    "
    set last [expr {[llength $cclass_pmap] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	append line [lindex $cclass_pmap $i]
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
 * The cclassGroupMap is indexed by combining the alternate page number with
 * the page offset and returns a combining class number.
 */

static unsigned char cclassGroupMap\[\] = {"
    set line "    "
    set lasti [expr {[llength $cclass_pages] - 1}]
    for {set i 0} {$i <= $lasti} {incr i} {
	set page [lindex $cclass_pages $i]
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

#define GetUniCharCClass(ch) (cclassGroupMap\[(cclassPageMap\[(((int)(ch)) & 0x1fffff) >> CCLASS_OFFSET_BITS\] << CCLASS_OFFSET_BITS) | ((ch) & ((1 << CCLASS_OFFSET_BITS)-1))\])


#define DECOMP_OFFSET_BITS $decomp_shift

/*
 * The pageMap is indexed by page number and returns an alternate page number
 * that identifies a unique page of characters.  Many Unicode characters map
 * to the same alternate page number.
 */

static unsigned char decompPageMap\[\] = {"
    set line "    "
    set last [expr {[llength $decomp_pmap] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	append line [lindex $decomp_pmap $i]
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
 * The decompGroupMap is indexed by combining the alternate page number with
 * the page offset and returns a group number that identifies a length and
 * shift of decomposition sequence in decompList
 */

static int decompGroupMap\[\] = {"
    set line "    "
    set lasti [expr {[llength $decomp_pages] - 1}]
    for {set i 0} {$i <= $lasti} {incr i} {
	set page [lindex $decomp_pages $i]
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
 * List of decomposition sequences
 */

static int decompList\[\] = {"
    set line "    "
    set last [expr {[llength $decomp_list] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	set val [lindex $decomp_list $i]

	append line [format "%d" $val]
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
 * This macro extracts the information about a character from the
 * Unicode character tables.
 */

#define GetUniCharDecompInfo(ch) (decompGroupMap\[(decompPageMap\[(((int)(ch)) & 0x1fffff) >> DECOMP_OFFSET_BITS\] << DECOMP_OFFSET_BITS) | ((ch) & ((1 << DECOMP_OFFSET_BITS)-1))\])

#define GetDecompShift(info) ((info) & 0xffff)
#define GetDecompLen(info) ((info) >> 16)


#define COMP_OFFSET_BITS $comp_shift

/*
 * The pageMap is indexed by page number and returns an alternate page number
 * that identifies a unique page of characters.  Many Unicode characters map
 * to the same alternate page number.
 */

static unsigned char compPageMap\[\] = {"
    set line "    "
    set last [expr {[llength $comp_pmap] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	append line [lindex $comp_pmap $i]
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

static int compGroupMap\[\] = {"
    set line "    "
    set lasti [expr {[llength $comp_pages] - 1}]
    for {set i 0} {$i <= $lasti} {incr i} {
	set page [lindex $comp_pages $i]
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
 * Lists of compositions for characters that appears only in one composition
 */

static int compFirstList\[\]\[2\] = {"
    set line "    "
    set last [expr {[llength $comp_first_list] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	set val [lindex $comp_first_list $i]

	append line [format "{%d, %d}" [lindex $val 0] [lindex $val 1]]
	if {$i != $last} {
	    append line ", "
	}
	if {[string length $line] > 60} {
	    puts $f $line
	    set line "    "
	}
    }
    puts $f $line
    puts $f "};

static int compSecondList\[\]\[2\] = {"
    set line "    "
    set last [expr {[llength $comp_second_list] - 1}]
    for {set i 0} {$i <= $last} {incr i} {
	set val [lindex $comp_second_list $i]

	append line [format "{%d, %d}" [lindex $val 0] [lindex $val 1]]
	if {$i != $last} {
	    append line ", "
	}
	if {[string length $line] > 60} {
	    puts $f $line
	    set line "    "
	}
    }
    puts $f $line
    puts $f "};

/*
 * Compositions matrix
 */

static int compBothList\[[llength $comp_x_list]\]\[[llength $comp_y_list]\] = {"
    set lastx [expr {[llength $comp_x_list] - 1}]
    set lasty [expr {[llength $comp_y_list] - 1}]
    for {set i 0} {$i <= $lastx} {incr i} {
	puts $f "    \{"
	set line "        "
	for {set j 0} {$j <= $lasty} {incr j} {
	    set comp [list [lindex $comp_x_list $i] [lindex $comp_y_list $j]]
	    if {[info exists comp_map($comp)]} {
		set val $comp_map($comp)
	    } else {
		set val 0
	    }
	    
	    append line [format "%d" $val]
	    if {$j != $lasty} {
		append line ", "
	    }
	    if {[string length $line] > 70} {
		puts $f $line
		set line "        "
	    }
	}
	puts $f $line
	if {$j != $lasty} {
	    puts $f "    \},"
	} else {
	    puts $f "    \}"
	}
    }
    puts $f "};


#define GetUniCharCompInfo(ch) (compGroupMap\[(compPageMap\[(((int)(ch)) & 0x1fffff) >> COMP_OFFSET_BITS\] << COMP_OFFSET_BITS) | ((ch) & ((1 << COMP_OFFSET_BITS)-1))\])

#define CompSingleMask (1 << 16)
#define CompMask ((1 << 16) - 1)
#define CompSecondMask (1 << 17)
"

    close $f
}

uni::main

return
