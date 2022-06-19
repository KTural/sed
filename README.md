# sed

## Info
#### sed Unix utility implementation with basic functionalities

## Sample Usage 

```
$ runhaskell sed.hs -h 
USAGE: runhaskell sed.hs OPTIONS... [SCRIPT] [INPUTFILE...]


Basic functionalities: 

      Replacing or substituting string: e.g.
              runhaskell sed.hs 's/unix/linux/' input.txt

      Replacing the nth occurrence of a pattern in a line: e.g.
              runhaskell sed.hs 's/unix/linux/2' input.txt

      Replacing all the occurrences of the pattern in a line: e.g.
              runhaskell sed.hs 's/unix/linux/g' input.txt

      Replacing string on a specific line number: e.g.
              runhaskell sed.hs '3 s/unix/linux/' input.txt

      Replacing string on a range of lines: e.g.
              runhaskell sed.hs '1,3 s/unix/linux/' input.txt

      Replacing the nth occurence of a pattern on a range of lines: e.g.
              runhaskell sed.hs '1,3 s/unix/linux/2' input.txt

      Replacing all the occurrences of the pattern on a range of lines: e.g.
              runhaskell sed.hs '1,3 s/unix/linux/g' input.txt

      Adding double pattern scripts to be run while processing the input : e.g.
              runhaskell sed.hs -e '1,3 s/unix/linux/g' -e '1,3 s/linux/unix/g' input.txt

      Loading double pattern scripts from the file to be run while processing the input : e.g.
              runhaskell sed.hs -f input.sed input.txt


Basic command-line options: 

      -n; --quiet; --silent 

      -e script
              add the commands in script to the set of commands to be run while processing the input

      -f script-file
              add the commands contained in the file script-file to the set of commands to be run while processing the input

      -h ; --help

```