# 50.054 Project Lab 2 (10%)

## Deadline - 1 Dec 2024 23:59

In the previous lab, we completed the parser and ir code generation.

If you have trouble completing lab 1, you can send the instructor an email to ask for the sample solution. 

In this lab, we look into the dynamic and static semantics of the SIMP language.

## Tasks for this Lab

The two main tasks for this lab include

1. Implementing an interpreter of SIMP programs.
1. Implementing a type inference for SIMP programs

There is no obvious dependency between the two tasks, hence you can divide the task among your team members if you wish to.

## Task 1 - SIMP interpreter

An interperter takes an ir of a program (AST, or Pseudo Assembly) and the input and runs the program ir form and returns the result.

Typically an interpreter implementation can derived based on the operational semantics. 

In `src/Lang/Simp/Interpreter/PAInt.hs`, we implemented an interpreter of Pseudo Assembly in Small Step Operational Semantics. We recommend you to study the code in `PAInt.hs` and `test/Lang/Simp/Interpreter/PAIntSpec.hs` to understand how the algorithm was implemented. There is no modification required to these two files.

As a backup and for practice purposes, we would like to implement an interpreter of SIMP (from SIMP AST). 

In `src/Lang/Simp/Interpreter/SIMPInt.hs`, we find the partially implemented interpreter using Big Step Operational Semantics of SIMP. 

### Sub Task 1.1

The `evalExp` function evaluates a SIMP expression to a constant by implementing the 
$\Delta \vdash E \Downarrow c$ rules introduced in the notes.

The function is nearly complete except that there are a few cases missing. 

Your task is to complete the implementation.

Upon completion, you should be able to pass the two test cases for `evalExp`.


```bash
cabal test --test-options="-m Lang.Simp.Interpreter.SimpIntSpec/SimpInt/evalExp"
```

or

```bash
stack build :simp-test --ta "-m Lang.Simp.Interpreter.SimpIntSpec/SimpInt/evalExp"
```

### Sub Task 1.2

The `eval` function evaluates a SIMP statement to a value environment by implementing the  $(\Delta, S) \Downarrow \Delta'$ rules.

Due to the definition of the statement AST, we need to overload `eval` for single statement and multiple statments.

Your task is to complete the implementation of the type class instances `Evaluable [a]` and `Evaluable Stmt`.


##### Testing the SIMP interpreter implementation 

Now you should be able to pass all test cases in `Lang.Simp.Interpreter.SimpIntSpec`


## Task 2 - SIMP Type Inference

In this task we look into the static semantics of the SIMP programming language.


The codes of concern in this task are in `src/Lang/Simp/Semantic/TypeInf.hs`

Recall the typing constructs for SIMP

$$
\begin{array}{rccl}
 {\tt (Variable)} & X & ::= & a \mid b \mid c \mid d \mid ... \\ 
 {\tt (Types)} & T & ::= & int \mid bool  \\ 
 {\tt (Extended\ Types)} & \hat{T} & ::=  &\alpha \mid T \\ 
 {\tt (Type\ Environments)} & \Gamma & \subseteq & (X \times T) \\ 
 {\tt (Constraints)} & \kappa & \subseteq & (\hat{T} \times \hat{T}) \\ 
 {\tt (Type\ Substitution)} & \Psi & ::= & [\hat{T}/\alpha] \mid [] \mid \Psi \circ \Psi 
\end{array}
$$

We encode the simple (non-extended) type as

```hs
data Type = IntTy | BoolTy 
    deriving (Show, Eq, Ord)
```

We encode the extended type as 

```hs
data ExType = MonoType Type | TypeVar String 
    deriving (Show, Eq, Ord)
```

then the type environment can be modelled via a dictionary mapping from variable to type.

```hs
type TypeEnv = DM.Map Var Type 
```

The type constraints can be modelled as a set of pairs of extended types.

```hs
type TypeConstrs = DS.Set (ExType, ExType)
```

Lastly the type substitution is a sequence of mapping type variable name to extended type.

```hs
data TypeSubst = Empty -- ^ [] 
    | RevComp (String, ExType) TypeSubst -- ^ psi compose [type/a]
    deriving (Show, Eq)
```

As per mentioned in the notes, during the type inference, we create skolem type variable $\alpha_x$ for (data value) variable $x$. In the implementation, we have `TypeVar "x"` as the skolem type variable for variable `Var "x"`, i.e. they share the same name.

### Sub Task 2.1 - Substitution

In this task, you need to implement the type substitution

$$
\begin{array}{rcll}
[]\hat{T} & = & \hat{T} \\ 
[\hat{T}/\alpha]\alpha & = & \hat{T} \\  
[\hat{T}/\alpha]\beta & = & \beta & if\ \alpha \neq \beta \\
[\hat{T}/\alpha]T & = & T
\end{array}
$$

Type substitution can be *compositional*.

$$
\begin{array}{rcll}
 (\Psi_1 \circ \Psi_2) \hat{T} & = & \Psi_1(\Psi_2(\hat{T}))
\end{array}
$$

To do that, we define a type class `Substitutable a` which allows us to overload the `applySubst` function to different parameter types.

Your task here is to complete the implementation of type class instance `Substitutable ExType`.

Upon completion, you should be able to pass the four test cases for `substitution`.

```bash
cabal test --test-options="-m Lang.Simp.Semantics.TypeInfSpec/TypeInf/substitution"
```

or

```bash
stack build :simp-test --ta "-m Lang.Simp.Semantics.TypeInfSpec/TypeInf/substitution"
```


### Sub Task 2.2 - Unification

The output of the type inference algorithm is a set of type constraints.

Solving the set of type constraint via unification produces the type substitution that grounds all the skolem type variables.

$$
\begin{array}{rcl}
mgu(int, int) & = & [] \\ 
mgu(bool, bool) & = & [] \\ 
mgu(\alpha, \hat{T}) & = & [\hat{T}/\alpha] \\ 
mgu(\hat{T}, \alpha) & = & [\hat{T}/\alpha] \\
\end{array}
$$

and

$$
\begin{array}{rcl}
mgu(\{\}) & = & [] \\ 
mgu(\{(\hat{T_1}, \hat{T_2})\} \cup \kappa ) & = & let\ \Psi_1 = mgu(\hat{T_1}, \hat{T_2}) \\ 
& & \ \ \ \ \ \ \kappa'  = \Psi_1(\kappa) \\ 
& & \ \ \ \ \ \ \Psi_2   = mgu(\kappa') \\ 
& & in\  \Psi_2 \circ \Psi_1  
\end{array}
$$

Since `mgu` needs to be overloaded, we define a type class `Unifiable a`. 

Your task is to complete the type class instances `Unifiable (ExType,ExType) ` and `Unifiable [a]`.

Upon completion, you should be able to pass the four test cases for `unification`.

```bash
cabal test --test-options="-m Lang.Simp.Semantics.TypeInfSpec/TypeInf/unification"
```

or

```bash
stack build :simp-test --ta "-m Lang.Simp.Semantics.TypeInfSpec/TypeInf/unification"
```


### Sub Task 2.3 - Inference

The type class instance method `infer` implements the $S \vDash \kappa$ rules.

The function `infExp` implements the $E \vDash \hat{T}, \kappa$ rules.

Your task is to complete the above two by implementing the missing cases.


#### Testing the Type Inference Implementation

Now you should be able to pass all test cases in `Lang.Simp.Semantics.TypeInfSpec`


## Everything So far

We should now be able to build an interpreter. 
Check out the code at `app/Main.hs`.

To build and use it, we have two options.

#### Option 1: Using Cabal 

```bash
cabal build
```

Let's say the last line of the build output in the console is 

```bash
[2 of 2] Linking /home/kenny/git/compilerdesign/project/haskell/simp/dist-newstyle/build/x86_64-linux/ghc-9.6.6/simp-0.1.0.0/x/simp/build/simp/simp [Objects changed]
```

Let's make a simple simp program called `fib.simp`

```java
x = input;
f = 0;
s = 1;
c = 0;
t = 0;
while c < x {
    t = f;
    f = s;
    s = t + f;
    c = c + 1;
}
return s;
```

Running 

```bash
./dist-newstyle/build/x86_64-linux/ghc-9.6.6/simp-0.1.0.0/x/simp/build/simp/simp -i fib.simp 5
```

should produce `8` as output.



#### Option 2: Using Stack

```bash
stack build
```

Let's say the last line of the build output in the console is 

```bash
simp> Installing executable simp in /home/kenny/git/compilerdesign/project/haskell/simp/.stack-work/install/x86_64-linux/44bc619c9746101ec8790b5e5e71dbd2a7a3cf0b5e156c6e3056070a13a95575/9.6.6/bin
```

Let's make a simple simp program called `fib.simp`

```java
x = input;
f = 0;
s = 1;
c = 0;
t = 0;
while c < x {
    t = f;
    f = s;
    s = t + f;
    c = c + 1;
}
return s;
```
Running 

```bash
.stack-work/install/x86_64-linux/44bc619c9746101ec8790b5e5e71dbd2a7a3cf0b5e156c6e3056070a13a95575/9.6.6/bin/simp -i fib.simp 5
```

should produce `8` as output.

