#!/usr/bin/env tclsh
#
# This script helps to automatically fill car models tables of each car maker
# in API spec.
#
# It parses car makers list from markdown spec and patches it back to add proper
# car model sections (with data obtained from the database) with rendered
# markdown tables.
#
# License (BSD-3):
#
#   Copyright (c) 2019 Viacheslav Lotsmanov
#
#   All rights reserved.
#
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions are met:
#
#       * Redistributions of source code must retain the above copyright
#         notice, this list of conditions and the following disclaimer.
#
#       * Redistributions in binary form must reproduce the above
#         copyright notice, this list of conditions and the following
#         disclaimer in the documentation and/or other materials provided
#         with the distribution.
#
#       * Neither the name of Viacheslav nor the names of other contributors
#         may be used to endorse or promote products derived from this
#         software without specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# constants
set MD_SPEC_FILE "Spec.md"
set CAR_MAKERS_TBL {"CarMake"}
set CAR_MODELS_TBL {"CarModel"}
set CAR_MAKERS_MARKER "#CAR_MAKERS_TABLE#"
set CAR_MODELS_BEGIN_SEPARATOR "#CAR_MODELS_BEGIN#"
set CAR_MODELS_END_SEPARATOR "#CAR_MODELS_END#"

# validating arguments
if { $argc != 1 || ![regexp {^postgresql://.+$} [lindex $argv 0]] } {
    set e "It's required to provide 'postgresql://…' argument with connection"
    set e "$e string of PostgreSQL database!"
    set e "$e Only this single argument is allowed to pass!"
    puts stderr $e
    exit 1
}

# opening permanent "psql" child process for read and write
set psql_connect [lindex $argv 0]
set psql [open "|psql -- $psql_connect 2>@stderr" r+]
fconfigure $psql -blocking 1 -buffering line

# executes a SQL query and parses result as an array of dictionaries
proc sql_query q {
    global psql
    puts $psql "$q;" ;# executing query

    # reading first two lines with column labels and separator line
    if { ![gets $psql heading] || ![gets $psql separator] } {
        error "Failed to read two rows of table head from SQL query response"
    }

    if ![regexp {^[-+]+$} $separator] {
        error "Something went wrong: separator is incorrect: $separator"
    }

    set col_names [split $heading "|"]
    for {set i 0} {$i < [llength $col_names]} {incr i} {
        lset col_names $i [string trim [lindex $col_names $i]]
    }

    set result_list [list]

    while {[gets $psql row] >= 0} {
        # check if it's the end of a result
        if [regexp {^\((\d+) rows?\)$} $row m rows_count] {
            if {$rows_count != [llength $result_list]} {
                set e "Something went wrong: total rows count "
                set e "$e doesn't match collected rows count (total:"
                error "$e $rows_count, collected: [llength $result_list])"
            }
            gets $psql row ;# flush empty last line
            break
        }

        # parsing a row of data
        set cols [split $row "|"]
        if {[llength $cols] != [llength $col_names]} {
            set e "Something went wrong: count of columns of a row"
            set e "$e ([llength $cols]) doesn't match column titles count"
            error "$e ([llength $col_names])"
        }
        for {set i 0} {$i < [llength $cols]} {incr i} {
            lset cols $i [string trim [lindex $cols $i]]
        }

        # filling result item (associated key-value array)
        for {set i 0} {$i < [llength $cols]} {incr i} {
            set item([lindex $col_names $i]) [lindex $cols $i]
        }

        lappend result_list [array get item]
    }

    return $result_list
}

# parses markdown of the spec and extracts predefined car makers.
# returns list of associated arrays (keys are "label" and "value").
proc parse_car_makers_dict spec_file {
    global CAR_MAKERS_MARKER
    set f [open $spec_file r]
    fconfigure $f -blocking 1 -buffering line
    set result [list]
    set car_makers_found 0

    # found columns indexes.
    # columns of car makers table allowed to be rearranged.
    set label_col_idx {}
    set value_col_idx {}

    proc clean_val x {
        set x [string trim $x]
        if [regexp {^`?(.*?)`?$} $x m v] {set x $v}
        return $x
    }

    while {[gets $f line] >= 0} {
        # in case we haven't found table yet, looking for separator
        if !$car_makers_found {
            if [regexp $CAR_MAKERS_MARKER $line] {
                set car_makers_found 1

                # there must be an empty line after separator
                gets $f line
                if {$line ne ""} {
                    set e "$spec_file: There must be an empty line after"
                    error "$e $CAR_MAKERS_MARKER marker"
                }

                # next two lines are table head
                gets $f heading; gets $f separator

                if ![regexp {^[ \-+|]+$} $separator] {
                    error "$spec_file: Markdown table separator is incorrect"
                }

                # looking only for label and value columns,
                # finding indexes of those columns.
                set col_names [split $heading "|"]
                for {set i 0} {$i < [llength $col_names]} {incr i} {
                    set x [string trim [lindex $col_names $i]]
                    if     [regexp {^Label($| )} $x] {set label_col_idx $i} \
                    elseif [regexp {^Value($| )} $x] {set value_col_idx $i}
                }

                if {$label_col_idx eq {} || $value_col_idx eq {}} {
                    error "$spec_file: Label and/or value columns are not found"
                }
            }
            continue
        }

        if {$line eq ""} break ;# empty line means it's end of the table

        # parsing only proper columns we need (value and label)
        set row [split $line "|"]
        set label_v [clean_val [lindex $row $label_col_idx]]
        set value_v [clean_val [lindex $row $value_col_idx]]

        set item(label) $label_v
        set item(value) $value_v
        lappend result [array get item]
    }

    if !$car_makers_found {
        set e "Failed to parse Car Makers from $spec_file,"
        error "$e $CAR_MAKERS_MARKER marker not found"
    }

    close $f
    return $result
}

# produces car models table for specific car maker.
# returns just string with rendered markdown heading and table.
proc render_car_models_table {car_make_raw car_models} {
    array set car_make $car_make_raw

    set label_th "Label (human-readable)"
    set value_th "Value (used in requests)"

    # obtaining fixed width of each column
    set label_cols [string length $label_th]
    set value_cols [string length $value_th]
    foreach x $car_models {
        array set car_model $x

        set len [string length $car_model(label)]
        if {$len > $label_cols} {set label_cols $len}

        set len [string length $car_model(value)]
        if {$len > $value_cols} {set value_cols $len}
    }

    # rendering heading, also adding an anchor we could use to jump to.
    set result \
        "#### <a name=\"car-models-of-${car_make(value)}-car-maker\"></a>"
    set result "${result}Car Models of \"${car_make(label)}\" Car Maker\n\n"

    # padding with spaces to make column to have fixed width
    proc fill_with_spaces {cols val} {
        while {[string length $val] < $cols} {set val "$val "}
        return $val
    }

    set label_th  [fill_with_spaces $label_cols $label_th]
    set label_sep [fill_with_spaces $label_cols -]
    set value_th  [fill_with_spaces $value_cols $value_th]
    set value_sep [fill_with_spaces $value_cols -]
    set result "${result}| $label_th | $value_th |\n"
    set result "${result}| $label_sep | $value_sep |"

    foreach x $car_models {
        array set car_model $x
        set label_v [fill_with_spaces $label_cols $car_model(label)]
        set value_v [fill_with_spaces $value_cols $car_model(value)]
        set result "$result\n| $label_v | $value_v |"
    }

    return $result
}
set car_makers [parse_car_makers_dict $MD_SPEC_FILE]

set rendered_md_tables [list] ;# list of strings
foreach x $car_makers {
    array set car_make $x

    # getting an id of the car maker
    set result [sql_query "
        SELECT id FROM $CAR_MAKERS_TBL
        WHERE value = '${car_make(value)}' LIMIT 1
    "]
    array set result_arr [lindex $result 0]
    set car_make(id) ${result_arr(id)}

    # getting all car models associated with this car maker.
    # alphabet order by label field.
    set result [sql_query "
        SELECT label, value FROM $CAR_MODELS_TBL
        WHERE parent = ${car_make(id)}
        ORDER BY label ASC
    "]
    lappend rendered_md_tables \
        [render_car_models_table [array get car_make] $result]
}

set all_rendered_car_models [join $rendered_md_tables "\n\n\\pagebreak\n\n"]

set f [open $MD_SPEC_FILE r]
fconfigure $f -blocking 1 -buffering line
set new_spec_contents ""
set is_car_models_opened 0

while {[gets $f line] >= 0} {
    if $is_car_models_opened {
        if [regexp $CAR_MODELS_END_SEPARATOR $line] {
            set is_car_models_opened 0
            set new_spec_contents \
                "${new_spec_contents}\n${all_rendered_car_models}\n\n"
            set new_spec_contents "${new_spec_contents}${line}\n"
        }
    } else {
        if [regexp $CAR_MODELS_BEGIN_SEPARATOR $line] {
            set is_car_models_opened 1
        }
        set new_spec_contents "${new_spec_contents}${line}\n"
    }
}

close $f

if $is_car_models_opened {
    set e "$MD_SPEC_FILE: $CAR_MODELS_BEGIN_SEPARATOR is opened but not closed"
    error "$e with $CAR_MODELS_END_SEPARATOR"
}

set f [open $MD_SPEC_FILE w]
fconfigure $f -blocking 1 -buffering line
puts -nonewline $f $new_spec_contents
close $f

close $psql
