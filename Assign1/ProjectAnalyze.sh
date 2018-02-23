#!/git-bash.exe

#change git-bash to bash.exe @NotAProfDalves, stuff is broken on my computer so only the github cli bash works

#Potential feature turning some of this stuff into HTML if -report flag in args?

#----Arugment Parsing----
#sourced from https://stackoverflow.com/questions/255898/how-to-iterate-over-arguments-in-a-bash-script
for arg in "$@"
do
	Nothing="Nothing" #here to avoid errors for the moment
	#TODO stuff to parse the arguments
done




# random variable, not sure if I'll use it
UPTODATE=$(git pull)
#TODO fix this part

#----Find uncommitted changes I ----
#AKA feature # 1 adds the stuff form git status to the diff stuff


Changelog="changes.log" #var to change the file in the future if I ever use this thing again

#first sed cleans blank lines, second cleans mess in brackets, third cleans first line telling status of branch
STATUSCLEAN="/^ *$/d;/(*)/d;/^On branch*/,2d"
Test="/^ *$/d"

#sed '/^ *$/d' sourced from wikipedia on sed to clean empty lines
#sed '//,+#d' sourced from stackoverflow: https://stackoverflow.com/questions/4396974/sed-or-awk-delete-n-lines-following-a-pattern

rm "$Changelog"

#flag to see if there's untracked files
untracked=$( git status | grep "Untracked files:" | wc -l )

Status=$(git status)
Diff=$( git diff )

# clear changes.log
$( echo "The following files have not been committed" > "$Changelog")

# if there are untracked files
if [ $untracked -eq 1 ]
then
	#adds untracked tag to untracked files, after getting them
	$( echo "$Status" | sed -e "$STATUSCLEAN;/Untracked files:/d;/:/d;s/^	/	untracked:  /g" >> "$Changelog" )
	STATUSCLEAN="$STATUSCLEAN;/Untracked files:/,\$d" # delete all after line sourced from https://stackoverflow.com/questions/5227295/how-do-i-delete-all-lines-in-a-file-starting-from-after-a-matching-line 
fi

# get other changes
$( echo "$Status" | sed "$STATUSCLEAN;/Changes to be committed:/d;/Changes not staged for commit:/d" >> "$Changelog" )


#---- Find uncommitted changes II ----
#uses git diff prints in a cleaner style than default with lines numbered and all that


$(echo "Current git diff:" >> "$Changelog")

#iterate over lines https://superuser.com/questions/284187/bash-iterating-over-lines-in-a-variable

#init some vars for the read loop

#if in code fragment of diff, will use different stuffs
Code="False"
RCodeCount="0" # count current line of code
ACodeCount="0"
#Numbers to count code line
RNum="0"
ANum="0"
#padding altered from the following https://stackoverflow.com/questions/4409399/padding-characters-in-printf
#literally just a bunch of spaces
Pad=$(printf '%0.1s' " "{1..60})
#length to pad the numbers
PadL=6

#iterate over the lines of the diff stuff
#IFS keep leading spaces from https://stackoverflow.com/questions/29689172/bash-read-line-does-not-read-leading-spaces 
while IFS= read -r line; do
	if [ $ACodeCount -lt 1 -a $RCodeCount -lt 1 ]
	then
		if [ $Code = "True" ]
		then
			$(printf '%0.1s' "_"{1..25} >> "$Changelog") # so it's much clearer where the code fragments are
			$(echo >> "$Changelog") # nl cause printf%n doesn't work for some reason
		fi
		Code="False"
	fi

	#non code lines either of 5 types
	if [ "$Code" == "False" ]
	then
		#use multi sed command to get what exactly the non code line is
		Status=$(echo "$line" | sed -e "s/^diff --git.*/1/;s/^index.*/2/;s/^---.*/3/;s/^+++.*/4/;s/^@@.*/5/;s/[^1-5]/6/;s/\(.\).*/\1/;s/^$/6/")
		#1 = first line 2 = index objects 3 = --- 4 = +++ 5 = @@ aka start of code 6 = mode or something, too difficult to match easily
		if [ "$Status" -eq "5" ]
		then
			Code="True"
			RNum=$(echo "$line" | sed -e "s/^@@ -\([0-9]*\),.*/\1/")
			ANum=$(echo "$line" | sed -e "s/^@@ -[0-9]*,[0-9]* +\([0-9]*\),.*/\1/")
			RCodeCount=$(echo "$line" | sed -e "s/^@@ -[0-9]*,\([0-9]*\) .*/\1/")
			ACodeCount=$(echo "$line" | sed -e "s/^@@ -[0-9]*,[0-9]* +[0-9]*,\([0-9]*\).*/\1/")
		elif [ "$Status" -eq "3" ]
		then
			Rem=$(echo "$line" | sed -e "s/^--- a\///;s:/:\\\/:g") # file removed to fix def/null issue
		elif [ "$Status" -eq "4" ]
		then
			# dump the file name into the changelog
			$(echo >> "$Changelog")
			$(echo "$line" | sed -e "s/^+++ b\//File diff: /;s/+++ \/dev\/null/File deleted: $Rem/" >> "$Changelog" )
		fi

	elif [ $Code == "True" ]
	then
		#Print lines, numbering etc
		Start=$(echo $line | sed -e "s/\(.\).*/\1/")
		if [ "$Start" = "+" ]
		then
			PadA=$(( $PadL - ${#ANum} ))
			$(printf '%*.*s%s|%*.*s%s|%s' 0 "$PadL" "$Pad" "" 0 "$PadA" "$Pad" "$ANum" "$line" >> "$Changelog")
			ANum=$(( $ANum + 1 ))
			ACodeCount=$(( $ACodeCount - 1 ))
		elif [ "$Start" = "-" ]
		then
			PadR=$(( $PadL - ${#RNum} ))
			$(printf '%*.*s%s|%*.*s%s|%s' 0 "$PadR" "$Pad" "$RNum" 0 "$PadL" "$Pad" "" "$line" >> "$Changelog")
			RNum=$(( $RNum + 1 ))
			RCodeCount=$(( $RCodeCount - 1 ))
		else
			PadR=$(( $PadL - ${#RNum} ))
			PadA=$(( $PadL - ${#ANum} ))
			$(printf '%*.*s%s|%*.*s%s|%s' 0 "$PadR" "$Pad" "$RNum" 0 "$PadA" "$Pad" "$ANum" "$line" >> "$Changelog")
			#increment decrement stuff
			RNum=$(( $RNum + 1 ))
			ANum=$(( $ANum + 1 ))
			RCodeCount=$(( $RCodeCount - 1 ))
			ACodeCount=$(( $ACodeCount - 1 ))
		fi
			
		$(echo >> "$Changelog") #to add newline
	fi

done <<< $Diff
#---- FIND TODO ----


PadL=5 # new padding length for this part

TodoLog="todo.log"
> $TodoLog #clears log

Todo=$(grep -rnIE --exclude=ProjectAnalyze.sh "//TODO|#TODO") #//TODO cause I use java a lot

LastFile=">>Null" #>> cause that's illegal in filenames
while IFS= read -r line; do
	if [ -n "$line" ]
	then
		#get the general content from the grep strings
		File=$(echo "$line" | sed -re "s/([^:]*).*/\1/")
		LineNum=$(echo "$line" | sed -re "s/[^:]*:([0-9]*):.*/\1/")
		Content=$(echo "$line" | sed -re "s/[^:]*:[0-9]*:(.*)/\1/")
		if [ "$File" != "$LastFile" ]
		then
			echo "File: $File" >> "$TodoLog"
			LastFile=$File
		fi
		PadN=$(( $PadL - ${#LineNum} ))
		$(printf '%0.*s%s|%s' "$PadN" "$Pad" "$LineNum" "$Content" >> "$TodoLog")
		echo >> "$TodoLog"
	fi

done <<< $Todo

#TODO seriously cleanup the messy output of grep, file headings, and then the lines found similar format to diff

#----Count Code Lines----
#Cause literally this is a command I use all the time to see how close I am to 10k lines

echo "Lines of code = $(find -name "*.hs" -print0 | xargs -0 wc -l )"

#----Haskell Eror Check----

ErrorLog="error.log"

AllValid="False"

while [ "$AllValid" = "False" ]
do
	HSErr=ghc -fno-code *.hs &>> "$ErrorLog"
	
done

