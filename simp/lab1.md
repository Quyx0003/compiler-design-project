# 50.054 Project Lab 1 (10%)

## Deadline - 17 Nov 2024 23:59

In this project, we will develop a compiler for the SIMP programming language.

## Project template

The given project template consists the following major files and folders.

* `simp.cabal` - the cabal project description file. You should not modify this, as you are not allowed to use other external libraries other than those have been specified in this file.
* `cabal.project` - the extra cabal configuration stating that this project requires a sub project `haskell-wasm`. 
* `stack.yaml` - the stack project configuration file allows us to use `stack` besides `cabal`. 
* `src` - the code folder containing the source code.
* `test` - the code folder containing the test code. 


### Inside `src/Lang/Simp`
Inside this folder, we have the following sub folders

1. `Backend` - codes and mouldes for the code generation (lab 3)
1. `Interpreter` - codes and modules for the interpreter implementation (lab 2)
1. `IR` - codes and modules for the intermedia representation (lab 1 and lab 2)
1. `Semantics` - codes and modules for semantic analyses and type checking (lab 2 and lab 3)
1. `Syntax` - codes and modules for syntax analyses, i.e. lexing and parsing (lab 1)

### Tasks for this Lab

The two main tasks for this lab include

1. Parsing SIMP programs.
1. Generating Intermediate Representation (i.e. Pseudo Assembly) from a SIMP program.

There is no obvious dependency between the two tasks, hence you can divide the task among your team members if you wish to.

#### Task 0

Run `cabal build` (or `stack build`) and make sure there is no compilation error.

#### Task 1 - Complete the parser for SIMP programs

Recall that in the compiler pipeline

```mermaid
graph LR
A[Lexing] -->B[Parsing] --> C[Semantic Analysis] --> D[Optimization] --> E[Target Code Generation]
```
##### Lexing 
The first step is to perform *lexing*. Lexing is to scan through the input source file to ensure the text is constructed as a sequence of valid tokens specified by the syntax rules of the source langugage.

Recall that the SIMP language has the following grammar rules.

$$
\begin{array}{rccl}
(\tt Statement) & S & ::= & X = E ; \mid return\ X ; \mid nop; \mid if\ E\ \{ \overline{S} \}\ else\ \{ \overline{S} \} \mid while\ E\ \{ \overline{S} \} \\
(\tt Expression) & E & ::= & E\ OP\ E \mid X \mid C \mid (E) \\
(\tt Statements) & \overline{S} & ::= & S \mid S\ \overline{S} \\
(\tt Operator) & OP & ::= & + \mid - \mid * \mid < \mid == \\ 
(\tt Constant) & C & ::= & 0 \mid 1 \mid 2 \mid ... \mid true \mid false \\ 
(\tt Variable) & X & ::= & a \mid b \mid c \mid d \mid ... 
\end{array}
$$

At the moment, we do not handle division, as it might cause division by zero run-time errors.

The lexer for SIMP program has been implemented for you in `src/Lang/Simp/Syntax/Lexer.hs`. 

On the high level, we consider the set of lexer tokens contains all terminal symbols in the above grammar.

```hs
data LToken = EqSign SrcLoc  -- ^ = 
    | DEqSign SrcLoc         -- ^ ==
    | PlusSign SrcLoc        -- ^ + 
    | MinusSign SrcLoc       -- ^ - 
    | AsterixSign SrcLoc     -- ^ *
    ... --rest of the cases omitted
```

Where the `SrcLoc` refers to the line and column coordinate of the token in the source file. `SrcLoc` is defined in 
`SrcLoc.hs` under the same folder.

The top level function for the lexer is `lex::Parser LEnv [LToken]` which is a monadic parser that extract character sequence from the lexer environment `LEnv` and returns a list of lexer tokens. The entire lexer is implemented using monadic parser combinator. It has to make reference to 
the parser combinator library `src/Lang/Simp/Syntax/Parsec.hs`.

There is no need to modify these files.

You can test the lexer by running the test cases in  `test/Lang/Simp/Syntax/TestLexer.hs`, via the following command

```bash
cabal test --test-options="-m Lang.Simp.Syntax.LexerSpec"
```

or 

```bash
stack build :simp-test --ta "-m Lang.Simp.Syntax.LexerSpec"
```
You are recommended to study these test cases to get familiar with the input and output of the lexer. 

##### Parsing
The parser codes can be found in `src/Lang/Simp/Syntax/Parser.hs`. Its main task is to consume the output from the lexer, i.e. `[LToken]` and produce an abstract syntax tree. The abstract syntax tree data types can be found in `src/Lang/Simp/Syntax/AST.hs`

```hs
-- | The Statement data type 
data Stmt = Assign Var Exp -- ^ The constructor for an Assignment statement 
    | If Exp [Stmt] [Stmt] -- ^ The constructor for an If-else statement 
    | Nop                  -- ^ The constructor for a NOP statement 
    | While Exp [Stmt]     -- ^ The constructor for a While statement
    | Ret Var              -- ^ The consturctor for a Return statement
    deriving (Show, Eq) 


newtype Var = Var String deriving (Show, Eq, Ord) 

-- | The Expression data type
data Exp = Plus Exp Exp -- ^ The constructor for a plus expression e1 + e2
    | Minus Exp Exp     -- ^ The constructor for a subtraction expression e1 - e2          
    | Mult Exp Exp      -- ^ The constructor for a multiplication expression e1 * e2
    | DEqual Exp Exp    -- ^ The constructor for an equality test expression e1 == e2
    | LThan Exp Exp     -- ^ The constructor for a less-than test expression e1 < e2
    | ConstExp Const    -- ^ The constructor for a constant expression, e.g. 1, True 
    | VarExp Var        -- ^ The constructor for a variable expression, e.g. x, foo 
    | ParenExp Exp      -- ^ The constructor for a parenthese expression, ( e )
    deriving (Show, Eq)

-- | The Constant data type 
data Const = IntConst Int   -- ^ The constructor for an integer constant 
    | BoolConst Bool        -- ^ The constructor for a boolean constant
    deriving (Show, Eq) 
```

The parser is designed and implemented using the monadic parser combinator library too. It consists of a few important functions and data types

1. `PEnv` - the parsec state object which keeps track of the input `[LToken]`.
1. `pStmts` - the parser that parses a sequence of SIMP statements.
1. `pExp` - the parser that parses a SIMP expression.
1. and many other small parsers that form parts of the the main parsing routine.

The Parser codes are nearly complete. Except for the following 

###### Sub Task 1.1

Complete the definition of the `pSpace` parser which parses one white space token.

Complete the definition of the `pSpaces` parser which parses multiple white space tokens.

Upon completion, you should be able to pass the four test cases for `pSpaces`.

```bash
cabal test --test-options="-m Lang.Simp.Syntax.ParserSpec/parser/pSpaces"
```

or

```bash
stack build :simp-test --ta "-m Lang.Simp.Syntax.ParserSpec/parser/pSpaces"
```

Hint: if you forget how to use parser combinator library, refer to the code in the notes and in `Lexer.hs`.

###### Sub Task 1.2

Complete the definition of the `pExp` parser which parses an SIMP expression. 

Note that the grammar rule for expression contains a left-recursion. 

You should 

1. apply the left-recursion elimination to generate a left-recursion-free equivalent grammar for the expression. 
1. define the needed parser and sub parsers that parse the input using the left-recursion-free grammar.
1. convert the resulted expression AST in the left-recursion-free grammar back to the AST in the left-recursion grammar.

Note that the grammar rule for expression is also ambiguous. e.g. `1 + 2 * 3` can be parsed as 

1. `Plus (ConstExp (IntConst 1)) (Mult (ConstExp (Intconst 2))) (ConstExp 3)))` or 
1. `Mult (Plus (ConstExp (IntConst 1)) (ConstExp (Intconst2))) (ConstExp 3))` 

by applying left- or right-associativity or changing the grammar to respect the operator precedence.

For simplicity, we accept any diasmbiguation policies mentioned above. **In the test cases, we will always use parentheses to make the nesting explicit**.


##### Testing the Parser

Now you should be able to pass all test cases in `Lang.Simp.Syntax.ParserSpec`


#### Task 2 - Implement the Maximal Munch Algorithm v2.

In lecture we learn how to use Maximal Munch Algorithm to generate pseudo assembly language from SIMP.

The AST data structure of the pseudo assembly is defined in `src/Lang/Simp/IR/PseudoAssembly.hs`

```hs
type LabeledInstr = (Label, Instr)
type Label = Int 

data Instr = IMove Opr Opr
    | IPlus Opr Opr Opr
    | IMinus Opr Opr Opr
    | IMult Opr Opr Opr
    | IDEqual Opr Opr Opr
    | ILThan Opr Opr Opr
    | IRet
    | IIfNot Opr Label
    | IGoto Label
    deriving (Show, Eq)

data Opr = Regstr String
    | IntLit Int
    | Temp AVar
    deriving (Show, Eq, Ord) 


newtype AVar = AVar {name::String} deriving (Show, Eq, Ord) 
```

The original Maximal Munch Algorithm is implemented in `src/Lang/Simp/IR/MaximalMunch.hs`. 

It is implemented using
* a state monad `State StateInfo` - which enables us to generate new variable names without clashing, generate new label for the instruction.

The basic util functions of `State StateInfo` include

* `get` - reads the current state
* `put` - updates the state

And some extended util functions are as follow

* `newName` - generate a unique new string
* `newTemp` - generate a new temp variable
* `mkRegstr` - create a new register
* `chkNextLabel` - query for  the next label

You may study `MaximalMunch.hs` to get yourself familiar with the usable of the monad. 
You may also study `test/Lang/Simp/IR/MaximalMunchSpec.hs` to understand how the IR is generated.

##### Sub Task 2.1

Your task here is to complete the implementation of Maximal Munch v2 algorithm in the file `src/Lang/Simp/IR/MMUpDown.hs`. 


The function `cogenExp` implements the $G_e(E) \vdash (\^{e}, \v{e})$ rules. There are some cases missing. Your should implement the few missing cases. 

Upon completion, you should be able to pass the two test cases for `cogenExp`.



```bash
cabal test --test-options="-m Lang.Simp.IR.MMUpDownSpec/mm_updown/cogenExp"
```

or

```bash
stack build :simp-test --ta "-m Lang.Simp.IR.MMUpDownSpec/mm_updown/cogenExp"
```


##### Sub Task 2.2 

The function `cogen` implements the $G_s(S) \vdash lis$ rules. There are some cases missing. Your should implement the few missing cases. 


##### Testing the Maximal Munch Algorithm v2 implementation 

Now you should be able to pass all test cases in `Lang.Simp.IR.MMUpDownSpec`

