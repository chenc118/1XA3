#!/git-bash.exe

# random variable, not sure if I'll use it
UPTODATE=$(git pull)

#first sed cleans blank lines, second cleans mess in brackets, third cleans first line telling status of branch
STATUSCLEAN="/^ *$/d;/(*)/d;/^On branch*/,2d"
Test="/^ *$/d"

#sed '/^ *$/d' sourced from wikipedia on sed to clean empty lines
#sed '//,+#d' sourced from stackoverflow: https://stackoverflow.com/questions/4396974/sed-or-awk-delete-n-lines-following-a-pattern


#flag to see if there's untracked files
untracked=$( git status | grep "Untracked files:" | wc -l )

# clear changes.log
$( > "changes.log")


if [ $untracked -eq 1 ]
then
	#adds untracked tag to untracked files, after getting them
	$( git status | sed -e "$STATUSCLEAN;/Untracked files:/d;s/^/untracked:/g" > "changes.log" )
	STATUSCLEAN="$STATUSCLEAN;/Untracked files:/,\$d" # delete all after line sourced from https://stackoverflow.com/questions/5227295/how-do-i-delete-all-lines-in-a-file-starting-from-after-a-matching-line 
fi

# get other changes
$( git status | sed "$STATUSCLEAN;/Changes to be committed:/d;/Changes not staged for commit:/d" >> "changes.log" )


