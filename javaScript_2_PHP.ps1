$javascriptContent = Get-Content javascript.tcl -Raw

$wordMap = @{
    '\"js\"' = '"php"'
    '\"var \$' = '"\$'
    '\"var\"' = '""'
    '\\nvar ' = '\n '
    '_var \"' = '_var "\$'
   '; var ' = '; '
    'javascript' = 'PHP'
   '\$coll_var.length' = 'count\($coll_var\)'
   '\$keys_var.length' = 'count\($keys_var\)'
   'Object.keys' = 'array_keys'
}

$phpContent = $javascriptContent

foreach ($key in $wordMap.Keys) {
    $phpContent = $phpContent -replace $key, $wordMap[$key]
}

# пришлось делать отдельно
$wordMap = @{
   'return \"\\\$' = 'return "$'
}

foreach ($key in $wordMap.Keys) {
    $phpContent = $phpContent -replace $key, $wordMap[$key]
}

$wordMap = @{
    '\$variable === ' = '\$$$variable === ' 
    'throw ' = 'throw new('
    'Not expected condition.\\";"' = 'Not expected condition.\");"'
    '\+ \$switch_var;' = '. \$$$switch_var);'
}
   
foreach ($key in $wordMap.Keys) {
    $phpContent = $phpContent -replace $key, $wordMap[$key]
}


$php_KeyWords = @"

__halt_compiler abstract and array as
break callable case catch class
clone const continue declare default
die do echo else elseif
empty enddeclare endfor endforeach endif
endswitch endwhile eval exit extends final
finally fn for foreach function global
goto if implements include include_once
instanceof insteadof interface isset list
match namespace new or print
private protected public require require_once
return static switch throw trait try
unset use var while xor yield

"@

# Заменяем текст между "variable keywords {" и "}"
$phpContent = $phpContent -replace "(?s)(?<=variable keywords {).*?(?=})", $php_KeyWords

$phpContent | Set-Content php.tcl
