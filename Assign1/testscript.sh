#!/git-bash.exe

Status=$(sed -e "s/^diff --git.*/1/;s/^index.*/2/;s/^---.*/3/;s/^+++.*/4/;s/^@@.*/5/;s/[^1-5]/6/;s/\(.\).*/\1/" < test.txt)
echo $Status
