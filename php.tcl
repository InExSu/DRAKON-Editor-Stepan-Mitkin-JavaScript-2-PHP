gen::add_generator "PHP" gen_php::generate

namespace eval gen_php {

variable keywords {
    if else elseif while for foreach function return break continue
    true false null
}

proc highlight { tokens } {
    variable keywords
    return [ gen_cs::highlight_generic $keywords $tokens ]
}

proc generate { db gdb filename } {
    global errorInfo
    
    set callbacks [ make_callbacks ]
    
    lassign [ gen::scan_file_description $db { header footer } ] header footer
    
    set diagrams [ $gdb eval {
        select diagram_id from diagrams } ]
        
    foreach diagram_id $diagrams {
        if { [ mwc::is_drakon $diagram_id ] } {
            set append_semicolon 1
            gen::fix_graph_for_diagram $gdb $callbacks $append_semicolon $diagram_id
        }
    }
    
    set nogoto 1
    set functions [ gen::generate_functions \
        $db $gdb $callbacks $nogoto ]
    
    if { [ graph::errors_occured ] } { return }
    
    set hfile [ replace_extension $filename "php" ]
    
    set f [ open_output_file $hfile ]
    catch {
        print_to_file $f $functions $header $footer
    } error_message
    set savedInfo $errorInfo
    
    catch { close $f }
    if { $error_message != "" } {
        puts $errorInfo
        error $error_message savedInfo
    }
}

proc make_callbacks { } {
    set callbacks {}
    
    gen::put_callback callbacks assign gen_php::assign
    gen::put_callback callbacks compare gen_php::compare
    gen::put_callback callbacks compare2 gen_php::compare
    gen::put_callback callbacks while_start gen_php::while_start
    gen::put_callback callbacks if_start gen_php::if_start
    gen::put_callback callbacks elseif_start gen_php::elseif_start
    gen::put_callback callbacks if_end gen_php::if_end
    gen::put_callback callbacks else_start gen_php::else_start
    gen::put_callback callbacks pass gen_php::pass
    gen::put_callback callbacks return_none gen_php::return_none
    gen::put_callback callbacks block_close gen_php::block_close
    gen::put_callback callbacks comment gen_php::comment
    gen::put_callback callbacks bad_case gen_php::bad_case
    gen::put_callback callbacks for_declare gen_php::foreach_declare
    gen::put_callback callbacks for_init gen_php::foreach_init
    gen::put_callback callbacks for_check gen_php::foreach_check
    gen::put_callback callbacks for_current gen_php::foreach_current
    gen::put_callback callbacks for_incr gen_php::foreach_incr
    gen::put_callback callbacks native_foreach gen_php::native_foreach
    gen::put_callback callbacks and gen_php::and
    gen::put_callback callbacks or gen_php::or
    gen::put_callback callbacks not gen_php::not
    gen::put_callback callbacks break "break;"
    gen::put_callback callbacks continue "continue;"
    gen::put_callback callbacks declare gen_php::declare
    gen::put_callback callbacks shelf gen_php::shelf
    
    gen::put_callback callbacks body gen_php::generate_body
    gen::put_callback callbacks signature gen_php::extract_signature
    
    return $callbacks
}

proc assign { variable value } {
    return "\$$variable = $value;"
}

proc compare { variable constant } {
    return "$variable == $constant"
}

proc while_start { } {
    return "while \(true\) \{"
}

proc if_start { } {
    return "if \("
}

proc elseif_start { } {
    return "\} else if \("
}

proc if_end { } {
    return "\) \{"
}

proc else_start { } {
    return "\} else \{"
}

proc pass { } {
    return ""
}

proc return_none { } {
    return "return;"
}

proc block_close { output depth } {
    upvar 1 $output result
    set line [ gen::make_indent $depth ]
    append line "\}"
    lappend result $line
}

proc comment { line } {
    if {[regexp {^item\s+\d+$} $line]} {
        return ""
    }
    return "// $line"
}


proc bad_case { switch_var select_icon_number } {
    if {[ string compare -nocase $switch_var "select" ] == 0} {
        return "throw new Exception\(\"Not expected condition.\"\);"
    } else {
        return "throw new Exception\(\"Not expected $switch_var\"\);"
    }
}

variable _foreach_meta
array set _foreach_meta {}

proc foreach_declare { item_id first second } {
    variable _foreach_meta
    
    set raw [ string trim $first ]
    if { $second ne "" } {
        append raw " ; "
        append raw [ string trim $second ]
    }
    
    set parts {}
    foreach p [ split $raw ";" ] {
        set p [ string trim $p ]
        if { $p ne "" } {
            lappend parts $p
        }
    }
    
    if { [ llength $parts ] > 0 } {
        set head [ lindex $parts 0 ]
        if { [ string equal -nocase $head "foreach" ] } {
            set parts [ lrange $parts 1 end ]
        } elseif { [ string match -nocase "foreach *" $head ] } {
            set head [ string trim [ string range $head 7 end ] ]
            set parts [ linsert [ lrange $parts 1 end ] 0 $head ]
        }
    }
    
    set count [ llength $parts ]
    if { $count == 2 } {
        set vars_str [ string trim [ lindex $parts 0 ] ]
        set items [ normalize_expr [ lindex $parts 1 ] ]
        
        set vars {}
        foreach v [ split $vars_str "," ] {
            set v [ string trim $v ]
            if { $v ne "" } {
                lappend vars [ normalize_var $v ]
            }
        }
        
        if { [ llength $vars ] == 1 } {
            set _foreach_meta($item_id) [ list mode single item [ lindex $vars 0 ] items $items ]
        } elseif { [ llength $vars ] == 2 } {
            set _foreach_meta($item_id) [ list mode kv key [ lindex $vars 0 ] value [ lindex $vars 1 ] items $items ]
        } else {
            set _foreach_meta($item_id) [ list mode none ]
        }
    } elseif { $count == 3 } {
        set key [ normalize_var [ lindex $parts 0 ] ]
        set value [ normalize_var [ lindex $parts 1 ] ]
        set items [ normalize_expr [ lindex $parts 2 ] ]
        set _foreach_meta($item_id) [ list mode kv key $key value $value items $items ]
    } else {
        set _foreach_meta($item_id) [ list mode none ]
    }
    
    return ""
}

proc foreach_init { item_id first second } {
    variable _foreach_meta
    
    if { ![ info exists _foreach_meta($item_id) ] } {
        return "while \(true\) \{"
    }
    
    set meta [ dict create {*}$_foreach_meta($item_id) ]
    set mode [ dict get $meta mode ]
    
    if { $mode eq "single" } {
        set item [ dict get $meta item ]
        set items [ dict get $meta items ]
        return "foreach \($items as $item\) \{"
    } elseif { $mode eq "kv" } {
        set key [ dict get $meta key ]
        set value [ dict get $meta value ]
        set items [ dict get $meta items ]
        # Генерируем PHP синтаксис с => для ключ-значение пар
        return "foreach \($items as $key => $value\) \{"
    }
    
    return "while \(true\) \{"
}


proc foreach_check { item_id first second } {
    variable _foreach_meta
    
    if { ![ info exists _foreach_meta($item_id) ] } {
        return ""
    }
    
    set meta [ dict create {*}$_foreach_meta($item_id) ]
    set mode [ dict get $meta mode ]
    
    if { $mode eq "single" || $mode eq "kv" } {
        return "false"
    }
    
    return ""
}

proc foreach_current { item_id first second } {
    return ""
}

proc foreach_incr { item_id first second } {
    return ""
}

proc native_foreach { for_var for_expr } {
    set item [ normalize_var $for_var ]
    set items [ normalize_expr $for_expr ]
    return "foreach \($items as $item\) \{"
}

proc normalize_var { text } {
    set s [ string trim $text ]
    if { $s eq "" } {
        error "Empty variable name"
    }
    if { [ string index $s 0 ] ne "\$" } {
        set s "\$$s"
    }
    return $s
}

proc normalize_expr { text } {
    set s [ string trim $text ]
    if { $s eq "" } {
        error "Empty expression"
    }
    return $s
}

proc and { left right } {
    return "\($left\) && \($right\)"
}

proc or { left right } {
    return "\($left\) || \($right\)"
}

proc not { operand } {
    return "!\($operand\)"
}

proc declare { type name value } {
    set var "\$$name"
    if { $value == "" } {
        return "$var;"
    } else {
        return "$var = $value;"
    }
}

proc shelf { primary secondary } {
    return "\$$secondary = \$$primary;"
}

proc generate_body { gdb diagram_id start_item node_list items incoming } {
    set name [ $gdb onecolumn {
        select name from diagrams where diagram_id = :diagram_id
    } ]
    error "Diagram $name is too complex"
}

proc drop_empty_lines { pairs } {
    set result {}
    foreach pair $pairs {
        lassign $pair code comment
        if { $code != {} } {
            lappend result $pair
        }
    }
    return $result
}

proc get_return_type_and_arguments { pairs } {
    if { $pairs == {} } {
        set arguments {}
        set returns ""
    } else {
        set last [ lindex $pairs end ]
        set start [ lindex $last 0 ]
        if { [ string match "returns *" $start ] } {
            set arguments [ lrange $pairs 0 end-1]
            set returns [ string range $start 8 end ]
        } else {
            set arguments $pairs
            set returns ""
        }
    }
    
    return [ list $returns $arguments ]
}

proc extract_signature { text name } {
    set pairs_raw [ gen::separate_from_comments $text ]
    set pairs [ drop_empty_lines $pairs_raw ]
    
    lassign [ get_return_type_and_arguments $pairs ] returns parameters
    
    set type "procedure"
    set signature [ gen::create_signature $type {} $parameters $returns ]
    
    set error_message ""
    return [ list $error_message $signature ]
}

proc print_to_file { fhandle functions header footer } {
    set version [ version_string ]
    puts $fhandle "<?php"
    puts $fhandle ""
    puts $fhandle "declare\(strict_types=1\);"
    puts $fhandle ""
    puts $fhandle "// Autogenerated with DRAKON Editor $version"
    puts $fhandle ""
    puts $fhandle $header
    puts $fhandle ""
    
    foreach function $functions {
        lassign $function diagram_id name signature body
        puts $fhandle ""
        set declaration [ build_declaration $name $signature ]
        puts $fhandle $declaration
        set lines [ gen::indent $body 1 ]
        puts $fhandle $lines
        puts $fhandle "\}"
    }
    
    puts $fhandle ""
    puts $fhandle $footer
}

proc build_declaration { name signature } {
    lassign $signature type access parameters returns
    
    set result "function $name\("
    set params {}
    foreach parameter $parameters {
        set param_line [lindex $parameter 0]
        
        # Split only on the first '=' to separate declaration from default value
        set declaration_part $param_line
        set default_part ""
        
        if {[string first "=" $param_line] != -1} {
            set eq_index [string first "=" $param_line]
            set declaration_part [string trim [string range $param_line 0 [expr {$eq_index - 1}]]]
            set default_part [string trim [string range $param_line $eq_index end]]
        }
        
        # Now parse the declaration part (type and variable name)
        set parts [split $declaration_part]
        set pname ""
        set type_part ""
        
        # Find the variable (starts with $ or will be prefixed with $)
        for {set i 0} {$i < [llength $parts]} {incr i} {
            set part [lindex $parts $i]
            if {[string match "$*" $part] || [string match "*$*" $part]} {
                # Found the variable
                set pname $part
                if {![string match "$*" $pname]} {
                    set pname "\$$pname"
                }
                # Reconstruct: type(s) + variable
                set type_part [join [lrange $parts 0 [expr {$i - 1}]]]
                break
            }
        }
        
        # If we didn't find a variable with $, use the last part as variable
        if {$pname eq ""} {
            if {[llength $parts] > 0} {
                set pname [lindex $parts end]
                if {![string match "$*" $pname]} {
                    set pname "\$$pname"
                }
                set type_part [join [lrange $parts 0 end-1]]
            }
        }
        
        # Build the final parameter string
        set param_str ""
        if {$type_part ne ""} {
            append param_str "$type_part "
        }
        append param_str $pname
        if {$default_part ne ""} {
            append param_str " $default_part"
        }
        
        lappend params $param_str
    }
    
    append result [join $params ", "]
    if { $returns ne "" } {
        return "$result\): $returns \{"
    } else {
        return "$result\) \{"
    }
}



}
