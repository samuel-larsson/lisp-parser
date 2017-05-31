# Lisp parser




**Description**: An exercise in programming languages and semantics. It is a parser that reads and parses basic programs written in Pascal for syntactic and semantic errors. I also included the test suite in the folder **/testfiles** which contains all the test programs that the parser ran through.

[Exercise instructions](https://www.cs.kau.se/cs/education/courses/dvgc01/lab_info/index.php?lab3=1)

#### Dependencies:
* CLISP (macOS/Linux)

#### How to run the parser:
It runs the test suite by default, so to run a single program you have to uncomment the function:
```
;; (parse(filename))
```
Then run the parser:

macOS:
```
clisp < parser.lsp
```
