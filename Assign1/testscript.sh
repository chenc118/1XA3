#!/git-bash.exe

Something="--exclude=changes.log --exclude=todo.log"

grep -rnIE "$Something" "#TODO"
