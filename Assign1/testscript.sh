#!/git-bash.exe

PadLength=15
String="test"
printf '%0.*s%s' $PadLength "$(printf '%0.1s' " "{1..100})" "$String"
echo $Status
