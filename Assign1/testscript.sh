#!/git-bash.exe

non="changes.log"
Something+=("todo.log")
Something+=("$non")

Stat=$(git diff -- . "${Something[@]/#/\:\(exclude\)}")
echo $Stat