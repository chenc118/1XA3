#!/git-bash.exe

Something+=("todo.log")
Something+=("changes.log")

grep -rnIE ${Something[@]/#/--exclude=} "#TODO"
