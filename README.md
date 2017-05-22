
# TTyped

TTyped is a dependently typed language with hierarchical type universes. de Bruijn
indices are used for variable identification. There is one base type, which is
the unit type `ut`, whose value is denoted by `u`.

## Syntax

Comment lines start with a `#` and extend to the end of the line. Universes are
denoted by a `*` followed by an optional `{n}`, where `n` is the universe level.

### EBNF

```
expr = universe
     | '(' , pi , expr , '.' , expr , ')'
     | '(' , lambda , expr , '.' , expr , ')'
     | '(' , expr , expr , ')'
     | num ;

universe = '*' , [ '{' , num , '}' ] ;
num = digit , { digit } ;

pi = 'Π' | "||" ;
lambda = 'λ' | '\' ;
digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
```

## Semantics

TODO: Add evaluation semantics.

### Typing Rules

The following is a loose definition of the typing rules.

FIXME: Not sure what the proper way to denote variables is when using de Bruijn
indices. For now I'm using `0 : t` to denote a binding to a varaible of type `t`
and `P[0/t]` to denote substitution.

```
----------------------
 G |- *{n} : *{n + 1}

 G, t : *{n} |- 0 : t    G, 0 : t |- P : *{m}
---------------------------------------------- p = max n m
            G |- (Π t. P) : *{p}

 G, 0 : t |- P : *{n}    G, 0 : t |- e : P
-------------------------------------------
         G |- (λ t. e) : (Π t. P)

 G |- f : (Π t. P)    G |- e : t
---------------------------------
      G |- (f e) : P[0/e]

    G |-
-------------
 G |- u : ut

    G |-
-------------
 G |- ut : *
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
λ> ((\*{9}. 0) *{1})
Type Error: NoUnify (Universe 9) (Universe 2)
λ> # Type universes are not cumulative

λ> (\*. (\0. 0))
(λ *. (λ 0. 0)) : (Π *. (Π 0. 1))
λ> ((\*. (\0. 0)) ut)
(λ ut. 0) : (Π ut. ut)
λ> (((\*. (\0. 0)) ut) u)
u : ut

λ> (\*. (\(|| 0. 1). (0 0)))
Type Error: NoUnify (Var 1) (Pi (Var 1) (Var 2))
λ> (\*. (\(|| 0. 1). 0))
(λ *. (λ (Π 0. 1). 0)) : (Π *. (Π (Π 0. 1). (Π 1. 2)))
```
