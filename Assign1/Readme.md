## Assignment 1


### Usage

##### synoposis
ProjectAnalyze.sh [args]

Note if it does not run properly change the first line from `#!/git-bash.exe` to `#!/bash.exe` or wherever your bash installation is located

##### Args:
(args are denoted by `-` before the argument name listed below)
- autopull - Automatically pulls from the configured remote
- report - Renders error.log and changes.log in HTML, (minor difference w/ error.log but highlights additions/deletetions w/ changes.log)
- 1718 - Turns on feature # 7 (Will only work properly if in root directory)

#### Effects:
(Behavior may be modified by the arguments above)
1. Prints out whether the local branch is up to date with the remote branch
2. Puts the uncommited files, and the diffs into changes.log
3. Puts all lines w/ # TODO and // TODO into todo.log
4. Prints out the total # of lines of haskell code denoted by .hs files
5. Puts Haskell errors into error.log

#### Requirements fulfilled
1. Uses `git status` and `git fetch` to determine whether branch is up to date
2. Run `git diff` to get the uncommitted changes, outputs printed into changes.log w/ lines numbered and all that as they appear in the code source, also clean up some things
3. Gets todo with # and // in front
4. Dumps haskell errors into error.log
5. See features for all the "features" included, apparently the OP people are doing 5+

#### Additional Features
1. uncommitted files (including staged) will be displayed at the beginning of changes.log (cause I can't be bothered to just toss out those couple hours of work)
2. Literally every required feature is "fancy" making the input slightly more readable etc, and also resolving some errors
3. requirement 3 has // as well for practicality reasons as I code in java
4. report arg that changes the output from raw text to HTML (implemented for changes.log and error.log) and to make reading the output a lot more easier
5. Argument to auto pull(cause literally 99% of the time things won't break unless you're working as a group, then it's 50% of the time)
6. ~~Implmentation of https://www.xkcd.com/801/ (planned, got java code to modify for this)~~ Not enough time to actually implement
7. Implementation of https://xkcd.com/1718/ due to accidental ~~bug~~ feature which has resulted in the recursive growth of changes.log and todo.log (Trigger with `-1718` as an argument)
8. Prints out # of lines of Haskell code written

#### Code Summary:(Simplified)

- `1-35` (Argument parsing) - Parses arguments supplied into various variables
- `36-45` (Check Status) - Uses `git fetch` and `git status` to get current state of the main branch and filters for only line 2 using sed
- `46-51` (Autopull feature) - if autopull arg triggered will also use the git pull command
- `52-124` (Uncommitted changes I) - Uses git status to find what files have not been committed and adds those to changes.log. Uses various sed expressions and a loop iterating through the lines of `git status` to allow for greater control. If report arg is triggered will go into the true branch of various if statements to add various HTML elements
- `125-248` (Uncommitted changes II) - Uses git diff to find changes in tracked files. Uses a line by line loop iterating over the output. Has 1 main nestled if statement one branch to parse the headers and the other to print the modified code.
-  `249-285` (Find TODO) - Uses grep printing the line and file and a for each line iterator to parse each line into more readable output.
- `286-290` (Count Code Lines) - Uses find piping into xargs then wc then sed to find all haskell files, and count the total # of lines
- `291-331` (Haskell error check) - Uses a for each file loop to iterate over each hs file and `ghc -fno-code` to 1. determine if file has error. 2. a branch that adds main = undefined if the error is missing main, dropping that line afterwards

##### Citations:
See inline comments for where I got various parts

##### Notes:

Some things that are different from other's cause of the intended useage

1. No prompts cause I'll maybe add this to some sort of other script or program if I ever use it and have it execute w/ just cli args, prompts makes things easier to mess up, or any sort of REPL form of loop. (i.e. I've got old code that can scheduale stuff and maybe figure out how to get this to run after a push event from github i.e. webhooks being one way)
2. Does everything at once for the same above reason(easier to run as a part of some other code), may add args to supress certain behaviors(i.e. w/o `-1718` suppresses recursive growth of changes.log and todo.log)
3. No functions due to #1 each part only needs to be done once, for each section use CTRL+F `#----` which is used to mark the start of each section.
