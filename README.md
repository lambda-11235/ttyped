
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

TTyped a hierarchy of type universes.

TODO

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
