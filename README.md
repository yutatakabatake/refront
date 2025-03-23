# Refront

## What is Refront
Refront offers you an easy way to generate a *front-end* of your
formal definition of a language in PLT Redex (Redex in short here).

In Redex, the syntax of the language is defined in the form of
*S-expression*, which might cause inconveniences for those who are not
used to read/write programs in S-expressions. By using Refront, you
can obtain your custom front-end for the *newly-defined* and often
*required-to-be-tested-in-depth* language. Although not fully
automatic, you may easily define your own surface grammar and build a
parser to connect programs written in the surface language to the core
language defined in Redex, with hopefully limitted amount of burden.

## Requirements
You need the following tools:
- The Racket language processor.
- The Redex tool, which is a DSL in Redex.
- Your favorite parser generator.
  - Currently, SLLGEN and OCamlyacc/OCamllex have been tried.
	- SLLGEN: The EOPL package is required.
	- OCamlyacc/OCamllex: The OCaml language processor with
      ocamlyacc/ocamllex, and in addition, currently, `bison` is
      needed.
- Occasionally, you may need a bit of knowledge on some parsing
  algorithms; LL(1), LALR(1), etc.

## Version
Current version: 0.1.0

## Examples
### Preparation

1. Download the Refront tool somewhere.
Scripts in the `exe` directory are the drivers of the Refront tool.
You can put the directory on the `PATH` environment variable for convenience.
```
$ pwd
/somewhere/over/the/rainbow/refront
$ ls -F
README.md	exe/		lib/		samples/
$ ls exe
refront-build-ocaml.sh	refront-build-sllgen.sh
```

2. The `sampels` directory contains a few Redex files.
```
$ cd samples
$ ls *-lang.rkt
CALLCC-one-lang.rkt     Pretnar-lang.rkt
```

### Example 1
First, let's try the OCamlyacc/OCamllex version.

1. Run the refront-build script for OCamlyacc/OCamllex.
```
$ ../exe/refront-build-ocaml.sh -l Pretnar
LANG_NAME=Pretnar

Extracting syntax information from Pretnar-lang.rkt...
candidates : (c)
Enter start symbol : 
```

2. You got asked to put the start symbol of the surface grammar. A few candidates could be shown. Just type `c` here.
```
...
Enter start symbol : c
Checking parser.mly...
OK grammar
OK

Generating ast_to_sexpr.ml...
Generating Pretnar-interp.sh...
==========================================
Pretnar-interp.sh has been build.
  Usage : ./Pretnar-interp.sh program.txt
==========================================
$
```

3. The interpreter is ready. Let's run a program.
Note: intermediate files are built in the `build` directory.
If the directory does not exist, it will be created automatically.
`Pretnar-interp.sh` is a symbolic link to the file of the same name in the directory.
```
$ cat program-Bob.txt 
with handler {  return x -> return x
                read (_ ; k) -> do x <- apply k "Bob" in apply k x
                print (s ; k) -> apply k unit} 
handle do forename <- apply fun x -> read(x;y. return y) unit in 
        do surename <- apply fun x -> read(x;y. return y) unit in 
            return pair forename surename
$ ./Pretnar-interp.sh program-Bob.txt 
((return (pair (pair Bob (pair Bob Bob)) (pair (pair Bob (pair Bob Bob)) Bob))))
$
```


### Example 2

Next, let's try another example.

1. Run the refront-build script again for another language definition.

```
$ ../exe/refront-build-ocaml.sh -l CALLCC-one
LANG_NAME=CALLCC-one

Extracting syntax information from CALLCC-one-lang.rkt...
candidates : ((e env))
Enter start symbol : 
```

2. You got asked to put the start symbol of the surface grammar.
Type `e` here.
```
Enter start symbol : e
Checking parser.mly...
/Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly: warning: 157 shift/reduce conflicts [-Wconflicts-sr]
/Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly: warning: 42 reduce/reduce conflicts [-Wconflicts-rr]
/Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly: note: rerun with option '-Wcounterexamples' to generate conflict counterexamples

stop via reduce/reduce

Build procces stopped. Please check /Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly.
 1. Fix it (or a copied one) to make it LALR(1) in OCamlyacc format.
 2. Run this script again as follows:
  refront-build-ocaml.sh -l CALLCC-one -k [fixed_grammar_file_name]
```

3. You need to correct the surface grammar because the automatically generated temporary surface grammar contains reduce/reduce conflicts, which should be eliminated. Let's edit the file. The file is in the `build` directory and it's name is `parser.mly`. Suppose you found that (1) `| e e { E53_e {$1, $2}}` should be replaced with `| APP e e { E53_e {$2, $3}}`, (2) you need to append `APP` on the `%token` line, and add a definition of the non-terminal `APP` (i.e., insert the line `| "app" { APP }` in `lexer.mll`.
```
vi build/parser.mly
...
vi build/lexer.mll
...
```

4. Let's continue the process. When you modify the automatically generated temporary grammar files directly, you can just do that with `-k` option added.
```
$ ../exe/refront-build-ocaml.sh -l CALLCC-one -k
LANG_NAME=CALLCC-one

Checking parser.mly...
OK grammar
OK

Generating ast_to_sexpr.ml...
4 shift/reduce conflicts.
Generating CALLCC-one-interp.sh...
==========================================
CALLCC-one-interp.sh has been build.
  Usage : ./CALLCC-one-interp.sh program.txt
==========================================
$
```

5. The interpreter is ready. Let's run a program.
```
$ cat program-callcc-one.txt 
+ 5 
  app call/cc 
      lambda (k) 
        + app k
              2
          app k
              3
$ ./CALLCC-one-interp.sh program-callcc-one.txt
7
```


### Example 3

This time, let's try the SLLGEN version.

1. Run the refront-build script.
```
$ ../exe/refront-build-sllgen.sh -l CALLCC-one
LANG_NAME=CALLCC-one

Extracting syntax information from CALLCC-one-lang.rkt...
candidates : ((e env))
Enter start symbol : 
```

2. You got asked to put the start symbol of the surface grammar. Just type `e` here.
```
...
Enter start symbol : e
Checking CALLCC-one-fix-eopl-grammar.rkt...
Not LL(1) grammar!!!!!!
conflict productions :((e (e e) e96-e) (e (A e) A59-e))
Enter fixed production (or type "stop" to abort) :
```

3. You got another prompt to correct the grammar because the grammar defined in Redex is not LL(1).
Here, the message says that there are cases where which rule of `e ::= e e` and `e ::= e (A e)` to chose is undecidable looking at a lookahead symbol while parsing your program in a text file.
(TODO: make messages more readable)
You can type "stop" to suspend the interaction with Refront, edit the grammar file, and resume the process. But here,
let's try correcting interactively; type `(e ("app" e e) e96-e)` to add a terminal symbol in one of them. Note that the last element is an arbitrarily given identifier.
```
...
Enter fixed production (or type "stop" to abort) :(e ("app" e e) e96-e)
(e (app e e) e96-e)
(define grammar (quote ((program (e) a-program) (e (v) v1-e) (e (identifier) x6-e) (e (if e e e) if45-e) (e (zero? e) zero?89-e) (e (+ e e) +71-e) (e (* e e) *27-e) (e (let ( ( identifier e ) ) e) let31-e) (e (lambda ( identifier ) e) lambda3-e) (e (app e e) e96-e) (e (letrec ( ( identifier e ) ) e) letrec53-e) (e (A e) A59-e) (v (number) number75-v) (v (b) b-v) (b (true) true-b) (b (false) false-b) (v (call/cc) call/cc42-v) (v (closure identifier e env) closure37-v) (env ((arbno b identifier v)) boolean98-env))))

Not LL(1) grammar!!!!!!

parser-generation: grammar not LL(1): shift conflict detected for class "false" in nonterminal b31:
(("false" "true") (non-term b) (term identifier) (non-term v) (goto b31))
((end-marker number "call/cc" "closure" "A" "letrec" "app" "lambda" "let" "*" "+" "zero?" "if" identifier ")" "false" "true") (emit-list))

It is not apparent which rule to modify. Please review the grammar.

Build procces stopped. Please check /Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0322/refront/samples/build/CALLCC-one-fix-eopl-grammar.rkt.
 1. Fix it (or a copied one) to make it LL(1) in SLLGEN format.
 2. Run this script again as follows:
  refront-build-sllgen.sh -l CALLCC-one -k [fixed_grammar_file_name]
```

4. Oops! It seems that you need to modify other parts of the surface grammar, too. OK, Let's edit the temporary grammar directly. The file is in the `build` directory and it's name ends with `-fix-eopl-grammar.rkt`. Suppose you are knowledgable enough to find that the rule `env ::= ((arbno b identifier v)) boolean98-env)` should be replaced with `env ::= ((arbno b identifier v) ".") boolean98-env)`.

  - Note: The productions that use the non-terminal `env` are not actually used for defining the surface language but used only for defining reduction relations. Thus, you can delete them. If you delete (some of) them, you also need to delete the corresponding productions in the internal grammar definition, which resides in `CALLCC-one-eopl-grammar.rkt` in this case. By default, Refront takes all productions as needed because it is not easy to determin which productions are needless. (TODO: make the user to select productions to be used before invoking refront-build in some way)
```
vi build/CALLCC-one-fix-eopl-grammar.rkt
... (edit the file)
```

5. Let's continue the process.
When you modifiy the temporary grammar file directly, you can just do that 
invoking the same command with `-k` appended.
```
$ ../exe/refront-build-sllgen.sh -l CALLCC-one -k
LANG_NAME=CALLCC-one

Checking CALLCC-one-fix-eopl-grammar.rkt...
LL(1) grammar!!!!!OK

Generating CALLCC-one-ast-to-list.rkt...
Generating CALLCC-one-interp.sh...
==========================================
CALLCC-one-interp.sh has been build.
  Usage : ./CALLCC-one-interp.sh program.txt
==========================================
```

6. Congratulations! The interpreter is ready. Let's run a program.
```
$ cat program-callcc-one.txt 
+ 5 
  app call/cc 
      lambda (k) 
        + app k
              2
          app k
              3
$ ./CALLCC-one-interp.sh program-callcc-one.txt
7
```

### Example 4
1. Another example for OCamlyacc/OCamllex.
```
$ ../exe/refront-build-ocaml.sh -l CALLCC1   
LANG_NAME=CALLCC1

Extracting syntax information from CALLCC1-lang.rkt...
candidates : ((e e ...) x v (A e))
Enter start symbol : 
```

2. You got asked to put the start symbol of the surface grammar.
There are several candidates. Type 'e' here.
(TODO: Irrelevant candidates such as `A`, which is a terminal symbol, and
duplicate occurrences of the same symbols should be eliminated)
```
...
Enter start symbol : e
Checking parser.mly...
/Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly: warning: 25 shift/reduce conflicts [-Wconflicts-sr]
/Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly: warning: 21 reduce/reduce conflicts [-Wconflicts-rr]
/Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly: note: rerun with option '-Wcounterexamples' to generate conflict counterexamples
/Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly:36.11-34: warning: rule useless in parser due to conflicts [-Wother]
   36 | 	| e g757 { G757 $1 :: $2 }
      |           ^~~~~~~~~~~~~~~~~~~~~~~~

stop via reduce/reduce

Build procces stopped. Please check /Users/kawabata/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples/build/parser.mly.
 1. Fix it (or a copied one) to make it LALR(1) in OCamlyacc format.
 2. Run this script again as follows:
  refront-build-ocaml.sh -l CALLCC1 -k [fixed_grammar_file_name]
```

3. When this occurs, you need to modify `parser.mly` (and possibly `lexer.mll`).
`parser.mly` includes following productions corresponding to the Redex definition of `e ::= e e...`; apparently ambiguous.
```
e 
	: e g757	{ E63_e ($1, $2) }
...

g757 
	: { [] }
	| e g757 { G757 $1 :: $2 }
```

4. Let's modify `e ::= e e...` to `e ::= e (e...)` to make the grammar LALR(1). That is, replace the production `e g757	{ E63_e ($1, $2) }`
with `e LPARE g757 RPARE	{ E63_e ($1, $3) }`.  Then, you can build an interpreter.
```
(edit build/parser.mly)
$ ../exe/refront-build-ocaml.sh -l CALLCC1 -k
LANG_NAME=CALLCC1

Checking parser.mly...
OK grammar
OK

Generating ast_to_sexpr.ml...
2 shift/reduce conflicts.
Generating CALLCC1-interp.sh...
==========================================
CALLCC1-interp.sh has been build.
  Usage : ./CALLCC1-interp.sh program.txt
==========================================
```

5. This time, try to modify the grammar more than that. 
Replace the production `v : L LPARE g758 RPARE e { L63_v {$3, $5}}`
with `v : L g758 RARROW e { L63_v {$2, $4}}` and add a line `%token RARROW` to introduce a new terminal.
You also need to modify `build/lexer.mll`;
  - insert a line `| "->" { RARROW }` and,
  - change the string corresponding to the terminal `L`, that is, change `| "L" { L }` to `| "fun" { L }`.
After the modification, type `../exe/refront-build-ocaml.sh -l CALLCC1 -k` again.

6. Let's run a program.
It looks that this program invokes the continuation bound to the variable k twice, but the reduction relation of the language allows one-shot continuation only. On the other hand, the order of evaluation of +'s arguments is not defined (non-determinism involved). Thus, there are two possibilities of the result of the evaluation of the program. 
```
$ cat program-CALLCC1.txt
+(5 call/cc(fun k -> +(k(2) k(3))))
kawabata@mbp2024-541: ~/Documents/Lab/CA/Projects/Refront/kawabata-test/0323/refront/samples 
$ ./CALLCC1-interp.sh ./program-CALLCC1.txt
(7 8)
$
```


## Authors
Yuta Takabatake, Hideyuki Kawabata, and Tetsuo Hironaka

## History
- March 2025: 1st version defined.

