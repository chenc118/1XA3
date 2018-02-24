### Assignment 1
Directory for assignment 1 stuffs

#### Usage

##### synoposis
ProjectAnalyze.sh [args]

##### Args:
(args are denoted by `-` before the argument name listed below)
- autopull - Automatically pulls from the configured remote
- report - Some log stuff rendered in HTML
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
4. report arg that changes the output from raw text to HTML (currently only implemented for Haskell errors)
5. Argument to auto pull(cause literally 99% of the time things won't break unless you're working as a group, then it's 50% of the time)
6. ~~Implmentation of https://www.xkcd.com/801/ (planned, got java code to modify for this)~~ Not enough time to actually implement
7. Implementation of https://xkcd.com/1718/ due to accidental ~~bug~~ feature which has resulted in the recursive growth of changes.log and todo.log 
8. Prints out # of lines of code written

##### Notes:

Some things that are different from other's cause of the intended useage

1. No prompts cause I'll maybe add this to some sort of other script thingy if I ever use it and have it execute w/ just cli args, prompts makes things easier to mess up, or any sort of REPL form of loop.
2. Does everything at once for the same above reason(easier to run as a part of some other code), may add args to supress certain behaviors.
