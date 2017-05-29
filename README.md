
# TTyped

TTyped is a dependently typed language with cumulative type universes. de Bruijn
indices are used for variable identification. This implementation also provides
finite types.

There is also a version based on the calculus of constructions in the branch
`CoC`.

## Syntax

Comment lines start with a `#` and extend to the end of the line. Universes are
denoted by a `*` followed by an optional `{n}`, where `n` is the universe level.
`F[n]` denotes a finite type with `n` elements, `[m,n]` (where m < n) denotes an
element of this type, and `finElim[n,l](C, ..., f)` is its eliminator which
eliminates to a value whose type has the type `*{l}`.

### EBNF

The core language that the type checker and reduce operate on has the grammar

```
expr = universe
     | '(' , pi , expr , '.' , expr , ')'
     | '(' , lambda , expr , '.' , expr , ')'
     | '(' , expr , expr , ')'
     | num
     | finexpr ;

finexpr = 'F', '[' , num , ']'
        | '[' , num , ',' , num , ']' ;
        | "finElim" , '[' , num , ',' , num , ']' , '(' , expr , { ',' , expr } , ',' , expr , ')' ;

universe = '*' , [ '{' , num , '}' ] ;
num = digit , { digit } ;

pi = 'Π' | "||" ;
lambda = 'λ' | '\' ;
digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
```

#### Binding Extensions

The core language is extended with bindings at the top level of the REPL and
files. These bindings have the form

```
repl = bind | expr ;

bind = sym , '=' , expr ;

sym = char , { char } ;

char = ? [a-zA-Z] ? ;
```

In addition `expr` is extended as shown below.

```
expr = ...
     | sym ;
```

## Semantics

TODO: Add evaluation semantics.

### Typing Rules

The following is a loose definition of the typing rules. TTyped uses cumulative
universes with subtyping.

TODO: Verify that subtyping relations are complete and correct. I'm pretty sure
there's a problem with the current implementation, but I don't know what it
could be.

`G` is t context, `G, x` denotes appending of a variable onto a context, and
`P/e` denotes substitution for the index 0. Also, `*` is short for `*{0}`.

```
--------
 A <: A

 A <: B    B <: C
------------------
      A <: C

 A <: B    G |- x : A
----------------------
      G |- x : B


----------------------
 G |- *{n} : *{n + 1}

-------------------- n <= m
 G |- *{n} <: *{m}


 G, t : *{n} |- x : t    G, x : t |- P : *{m}
---------------------------------------------- p = max n m
            G |- (Π t. P) : *{p}

 G |- t2 <: t1    G, x : t2 |- P1 <: P2
----------------------------------------
        G |- (Π t1. P1) <: (Π t2. P2)

 G, x : t |- P : *{m}    G, x : t |- e : P
-------------------------------------------
       G |- (λ t. e) : (Π t. P)

 G |- f : (Π t. P)    G |- e : t
---------------------------------
      G |- (f e) : P/e


-------------
 G |- F[N] : *

-----------------------------------------
 G |- [n,m] : F[m] if n < m else invalid

 G |- C : (Π F[n]. *{l})  G, C : (Π F[n]. *{l}) |- c<m> : (C [m, n])  G |- x : F[n]
------------------------------------------------------------------------------------
         G |- finElim[n,l](C, c<0>, c<1>, ..., c<n - 1>, x) : (C x)
```

## Example Session

```
> stack build
> stack exec ttyped

λ> u
u : ut
λ> ut
ut : *
λ> *
* : *{1}
λ> *{1}
*{1} : *{2}
λ> ((\*{9}. 0) *)
* : *{9}

λ> (\*. (\0. 0))
(λ *. (λ 0. 0)) : (Π *. (Π 0. 1))
λ> ((\*. (\0. 0)) ut)
(λ ut. 0) : (Π ut. ut)
λ> (((\*. (\0. 0)) ut) u)
u : ut

λ> (\*. (\(|| 0. 1). (0 0)))
Type Error: NotSubType (Pi (Var 1) (Var 2)) (Var 1)
λ> (\*. (\(|| 0. 1). 0))
(λ *. (λ (Π 0. 1). 0)) : (Π *. (Π (Π 0. 1). (Π 1. 2)))
```

Files that contain bindings can also be preloaded.

```
> stack exec ttyped lib/base.tt

λ> id
(λ *{1}. (λ 0. 0)) : (Π *{1}. (Π 0. 1))
λ> idT
(Π *{1}. (Π 0. 1)) : *{2}
λ> (id ut)
(λ ut. 0) : (Π ut. ut)
λ> ((id ut) u)
u : ut
λ> hasType
(λ *{1000}. (λ 0. 0)) : (Π *{1000}. (Π 0. 1))
λ> ((hasType idT) id)
(λ *{1}. (λ 0. 0)) : (Π *{1}. (Π 0. 1))
```

## TODO

- Add sigma, sum (disjunctive), identity, and well founded types.
