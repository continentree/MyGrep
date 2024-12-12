# MyGrep
A simple grep-like implementation in OCAML

To compile, use 
```shell
ocamlc -o mygrep regexp.ml
```

To use MyGrep, use the command line
```shell
./mygrep [pattern] [file]
```
The pattern use the postfix order. Here are some examples to have a better understanding on how to use MyGrep :

* "ab@" stands for the "ab" pattern.
* "ab|" stands for the "a" or the "b" patterns.
* "a*" stands for the "a", "aa", "aaa", ... , patterns.
* "." stands for "[any letter in ASCII]" pattern.

Important note : MyGrep process the file you want to search in line by line. That means that, if you want to search a word and not a line, add ".*" in order to overcome the presence of other characters before your word.

So, for example, if you want to look at every line that have the word "vache" or "voche" in a file called "landscape.txt", the command line you have to enter is :
```shell
./mygrep ".*v@ao|@c@h@e@.*@" "landscape.txt"
```