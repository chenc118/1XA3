#!/git-bash.exe

#change git-bash to bash.exe @NotAProfDalves, stuff is broken on my computer so only the github cli bash works

#Potential feature turning some of this stuff into HTML if -report flag in args?

#----Arugment Parsing----
#Static arg vars here to know what static vars there are, even though actually setting them is motly useless
Report="False"
Pull="False"
Moore="False"
#Modes to gather vars etc
Mode="Null"

#sourced from https://stackoverflow.com/questions/255898/how-to-iterate-over-arguments-in-a-bash-script
for arg in "$@"
do
	#starts with check done using https://stackoverflow.com/questions/2172352/in-bash-how-can-i-check-if-a-string-begins-with-some-value
	if [[ $arg == -* ]]
		then

		if [[ ${arg,,} = "-report" ]];then
			Report="True"
		elif [[ ${arg,,} = "-autopull" ]]; then
			Pull="True"
		elif [[ ${arg,,} = "-1718" ]]; then
			Moore="True"
		fi
		Mode="Null"
	#Modal check if argument accepts several values, otherwise gets appended to the GL Value var
	elif [[ $Mode = "Null" ]];then
		ArgValue+="$arg"
	fi
done

#----Check status----
git fetch >/dev/null # to consume output

Status=$(git status)

# https://stackoverflow.com/questions/6022384/bash-tool-to-get-nth-line-from-a-file
Status=$(echo "$Status" | sed -e "2q;d" )

echo $Status

#----AutoPull (feature)----
#if autopull flag set will try to autopull stuffs
if [[ $Pull = "True" ]]; then
	git pull
fi

#----Find uncommitted changes I ----
#AKA feature # 1 adds the stuff form git status to the diff stuff

TodoLog="todo.log"
Changelog="changes.log" #var to change the file in the future if I ever use this thing again

#first sed cleans blank lines, second cleans mess in brackets, third cleans first line telling status of branch
STATUSCLEAN="/^ *$/d;/(*)/d;/^On branch*/,2d"
Test="/^ *$/d"

#sed '/^ *$/d' sourced from wikipedia on sed to clean empty lines
#sed '//,+#d' sourced from stackoverflow: https://stackoverflow.com/questions/4396974/sed-or-awk-delete-n-lines-following-a-pattern
if [[ -e "$Changelog" ]];then
	rm "$Changelog"
fi


#flag to see if there's untracked files
untracked=$( git status | grep "Untracked files:" | wc -l )

if [[ $Moore = "False" ]];then
	DiffFilter+=("$Changelog")
	DiffFilter+=("$TodoLog")
fi
Status=$(git status)
Diff=$( git diff -- . "${DiffFilter[@]/#/\:\(exclude\)}" 2>/dev/null ) # Consume the error (warning LF replaced by CRLF on windows)

if [[ $Report = "True" ]];then
	#Pre wrapping whitespace from https://css-tricks.com/snippets/css/make-pre-text-wrap/
	#Better highlighting https://stackoverflow.com/questions/5589958/html-span-tag-stretch-to-avaliable-width
	echo "<!DOCTYPE html> <html><head><title>Project Diffs</title><style>.Uncommitted{color:#007E00}span{display:inline-block;width:100%}pre{white-space:pre-wrap}.ZeroMP{margin:0;padding:0}.Untracked{color:#AE6666}hr{border-color:black}.Diffbody{background-color:#CCCCCC}h3{text-align:center;text-decoration:underline}.Delete{background-color:#FFAAAA}.Add{background-color:#AAFFAA}</style></head>" >> "$Changelog"
fi

if [[ $Report = "True" ]];then
	echo "<h3>Uncommitted Files</h3>" >> "$Changelog"
	echo "<div class = \"ZeroMP\"><pre>" >> "$Changelog"
else
	$( echo "The following files have not been committed" >> "$Changelog")
fi

# if there are untracked files
if [ $untracked -eq 1 ]
then
	#adds untracked tag to untracked files, after getting them

	Untrack=$( echo "$Status" | sed -e "$STATUSCLEAN;/Untracked files:/d;/:/d;s/^	/	untracked:  /g" )
	
	if [[ $Report = "True" ]];then
		echo "</pre></div><div class =\"Diffbody Untracked\"><pre> $Untrack </pre></div>" >> "$Changelog"
	else
		echo "$Untrack" >> "$Changelog"
	fi
	STATUSCLEAN="$STATUSCLEAN;/Untracked files:/,\$d" # delete all after line sourced from https://stackoverflow.com/questions/5227295/how-do-i-delete-all-lines-in-a-file-starting-from-after-a-matching-line 
fi

# get other changes
StatCleaned=$( echo "$Status" | sed "$STATUSCLEAN" )

while IFS= read -r line; do
	if [[ $line =~ .*Changes\ to\ be\ committed.* ]];then
		if [[ $Report = "True" ]];then
			echo "</pre></div><h3>Files Not yet committed</h3><div class = \"Diffbody Uncommitted\"><pre>" >> "$Changelog"
		fi
	elif [[ $line =~ .*Changes\ not\ staged\ [f]or\ commit.* ]];then
		if  [[ $Report = "True" ]];then
			echo "</pre></div><h3>Files Not yet Staged</h3><div class = \"Diffbody Uncommitted\"><pre>" >> "$Changelog"
		fi
	else 
		echo "$line" >> "$Changelog"
	fi
done <<< $StatCleaned 


#---- Find uncommitted changes II ----
#uses git diff prints in a cleaner style than default with lines numbered and all that

if [[ $Report = "True" ]];then
	echo "</pre></div><h3>Files Diff (Excluding Untracked Files)</h3><div class = \"Diffbody\"><pre>" >> "$Changelog"
else
	$(echo "Current git diff:" >> "$Changelog")
fi
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
Pad=$(printf '%0.1s' " "{1..60}) # This is resused for TODO
#length to pad the numbers
PadL=6

#iterate over the lines of the diff stuff
#IFS keep leading spaces from https://stackoverflow.com/questions/29689172/bash-read-line-does-not-read-leading-spaces 
while IFS= read -r line; do
	if [ $ACodeCount -lt 1 -a $RCodeCount -lt 1 ]
	then
		if [ $Code = "True" ]
		then
			if [[ $Report = "True" ]];then
				printf "%s" "<hr width = \"100%\">" >> "$Changelog" # printf to preserve pre formatting
			else
				$(printf '%0.1s' "_"{1..25} >> "$Changelog") # so it's much clearer where the code fragments are
				$(echo >> "$Changelog") # nl cause printf%n doesn't work for some reason
			fi
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
			if [[ $Report = "True" ]];then
				echo "$line" | sed -e "s/^+++ b\//<h3>/;s/+++ \/dev\/null/<h3 class = \"Untracked\"> $Rem/;" >> "$Changelog"
				echo "</h3>" >> "$Changelog"
			else
				# dump the file name into the changelog
				$(echo >> "$Changelog")
				$(echo "$line" | sed -e "s/^+++ b\//File diff: /;s/+++ \/dev\/null/File deleted: $Rem/" >> "$Changelog" )
			fi
		fi

	elif [ $Code == "True" ]
	then
		#Print lines, numbering etc
		Start=$(echo $line | sed -e "s/\(.\).*/\1/")
		if [ "$Start" = "+" ]
		then
			PadA=$(( $PadL - ${#ANum} ))
			Diffl=$(printf '%*.*s%s|%*.*s%s|%s' 0 "$PadL" "$Pad" "" 0 "$PadA" "$Pad" "$ANum" "$line")
			if [[ $Report = "True" ]];then
				#HTML encoder from https://stackoverflow.com/questions/12873682/short-way-to-escape-html-in-bash
				Diffl=$(echo "$Diffl" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g')
				echo "<span class = \"Add\">$Diffl</span> " >> "$Changelog"
			else
				echo "$Diffl" >> "$Changelog"
			fi
			ANum=$(( $ANum + 1 ))
			ACodeCount=$(( $ACodeCount - 1 ))
		elif [ "$Start" = "-" ]
		then
			PadR=$(( $PadL - ${#RNum} ))
			Diffl=$(printf '%*.*s%s|%*.*s%s|%s' 0 "$PadR" "$Pad" "$RNum" 0 "$PadL" "$Pad" "" "$line")
			if [[ $Report = "True" ]];then
				Diffl=$(echo "$Diffl" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g')
				echo "<span class = \"Delete\">$Diffl</span>" >> "$Changelog"
			else
				echo "$Diffl" >> "$Changelog"
			fi
			RNum=$(( $RNum + 1 ))
			RCodeCount=$(( $RCodeCount - 1 ))
		else
			PadR=$(( $PadL - ${#RNum} ))
			PadA=$(( $PadL - ${#ANum} ))
			Diffl=$(printf '%*.*s%s|%*.*s%s|%s' 0 "$PadR" "$Pad" "$RNum" 0 "$PadA" "$Pad" "$ANum" "$line")
			if [[ $Report = "True" ]];then
				Diffl=$(echo "$Diffl" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g')
				echo "<span>$Diffl</span>" >> "$Changelog"
			else
				echo "$Diffl" >> "$Changelog"
			fi
			#increment decrement stuff
			RNum=$(( $RNum + 1 ))
			ANum=$(( $ANum + 1 ))
			RCodeCount=$(( $RCodeCount - 1 ))
			ACodeCount=$(( $ACodeCount - 1 ))
		fi
	fi

done <<< $Diff


if [[ $Report = "True" ]];then
	echo "</pre></div></html>" >> "$Changelog"
fi

#---- FIND TODO ----


PadL=5 # new padding length for this part


> $TodoLog #clears log

if [[ $Moore = "False" ]]; then
	TIgnore+=("$Changelog")
	TIgnore+=("$TodoLog")
fi

TIgnore+=("ProjectAnalyze.sh")

# Idea taken from https://github.com/gwgundersen/gp/blob/master/gp.sh
Todo=$(grep -rnIE ${TIgnore[@]/#/--exclude=} "//TODO|#TODO" )
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

#----Count Code Lines----
#A command I use all the time to see how close I am to 10k lines

echo "Lines of code = $(find -name "*.hs" -print0 | xargs -0 wc -l | grep total | sed -e "s/[^0-9]*\([0-9]*\).*/\1/")"

#----Haskell Error Check----

ErrorLog="error.log"
> "$ErrorLog" # clear log ofc

#placeholder for stuff to edit the head of the html doc once I find a nice CSS stylesheet
if [[ $Report = "True" ]]; then
	echo "<!DOCTYPE html> <html><head><title>Haskell ErrorLog</title><style>.ZeroMP{margin:0;padding:0}.Errorbody{background-color:#CCCCCC}h3{text-align:center;text-decoration:underline}</style></head>" >> "$ErrorLog"
	echo "<body><div class=\"ZeroMP\"><pre>" >> "$ErrorLog"
fi

shopt -s nullglob
for hsFile in `find -name "*.hs" -type f` # last minute fix from https://stackoverflow.com/questions/14505047/loop-through-all-the-files-with-a-specific-extension
do
	#error capture from https://stackoverflow.com/questions/962255/how-to-store-standard-error-in-a-variable-in-a-bash-script
	HSErr=$(ghc -fno-code "$hsFile" 2>&1 >/dev/null)
	#contains test from https://stackoverflow.com/questions/229551/string-contains-in-bash answer # 2
	#some strange things w/ regex to avoid annoying escaping errors
	if [[ $HSErr =~ .*The\ IO\ action\ .main.\ is\ not\ defined\ [i]n\ module\ .Main..*  ]] 
	then
		# if main undefined, create it momentarily
		echo "main = undefined" >> "$hsFile"
		HSErr=$(ghc -fno-code "$hsFile" 2>&1 >/dev/null)
		# line drop from https://stackoverflow.com/questions/4881930/remove-the-last-line-from-a-file-in-bash
		head -n -1 "$hsFile" > tmp.hs; mv tmp.hs "$hsFile"
	fi
	if [[ -n $HSErr ]]; then
		echo >> "$ErrorLog"
		if [[ $Report = "True" ]]; then
			echo "</pre></div><h3> $hsFile </h3><div class = \"Errorbody\"><pre>" >> "$ErrorLog"
		else
			echo "File: $hsFile" >> "$ErrorLog"
		fi
		echo "$HSErr" | sed -e "/^ *$/d" >> "$ErrorLog"
	fi
done

if [[ $Report = "True" ]];then
	echo "</pre></div></body></html>" >> "$ErrorLog"
fi
