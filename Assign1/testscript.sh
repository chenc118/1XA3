#!/git-bash.exe

Status=$(ghc -fno-code test.hs 2>&1 >/dev/null)
if [[ $Status =~ .*The\ IO\ action\ .main.\ is\ not\ defined\ [i]n\ module\ .Main..*  ]]
then
	echo "Found"
fi

