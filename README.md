
# TTyped

TTyped is a dependently typed language Based on the calculus of constructions.
de Bruijn indices are used for variable identification.

There is also a version based on intuitionistic type theory in the branch
`martin_lof`.

## Syntax

Comment lines start with a `#` and extend to the end of the line.

### EBNF

The core language that the type checker and reduce operate on has the grammar

```
term = context | object ;

context = '*'
        | '(' , forall , term , '.' , context , ')' ;

object = nat
        | '(' , forall , term , '.' , object , ')' ;
        | '(' , lambda , term , '.' , object , ')' ;
        | '(' , object , object ')' ;

forall = '∀' | '@' ;
pi = 'Π' | "||" ;
lambda = 'λ' | '\' ;

nat = digit , { digit } ;
digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;
```

#### Binding Extensions

The core language is extended with bindings at the top level of the REPL and
files. These bindings have the form

```
repl = bind | object ;

bind = sym , '=' , object ;

sym = char , { char } ;

char = ? [a-zA-Z] ? ;
```

In addition `expr` is extended as shown below.

```
context = ...
        | sym ;

object = ...
       | sym ;
```

## Semantics

See "The Calculus of Constructions" by Coquand and Huet.

## Example Session

```
> stack build
> stack exec ttyped

λ> id = (\*. (\0. 0))
λ> id
(λ*. (λ0. 0)) : (∀*. (∀0. 1))
λ> idT = (@*. (@0. 1))
λ> (id idT)
(λ(∀*. (∀0. 1)). 0) : (∀(∀*. (∀0. 1)). (∀*. (∀0. 1)))
λ> ((id idT) id)
(λ*. (λ0. 0)) : (∀*. (∀0. 1))
```

Files that contain bindings can also be preloaded.

```
> stack exec ttyped lib/base.tt lib/nat.tt

λ> id
(λ*. (λ0. 0)) : (∀*. (∀0. 1))
λ> const
(λ*. (λ0. (λ*. (λ0. 2)))) : (∀*. (∀0. (∀*. (∀0. 3))))
λ> ((add two) three)
(λ*. (λ(∀0. 1). (λ1. (1 (1 (1 (1 (1 0)))))))) : (∀*. (∀(∀0. 1). (∀1. 2)))
λ> five
(λ*. (λ(∀0. 1). (λ1. (1 (1 (1 (1 (1 0)))))))) : (∀*. (∀(∀0. 1). (∀1. 2)))
λ> ((mult three) three)
(λ*. (λ(∀0. 1). (λ1. (1 (1 (1 (1 (1 (1 (1 (1 (1 0)))))))))))) : (∀*. (∀(∀0. 1). (∀1. 2)))
λ> nine
(λ*. (λ(∀0. 1). (λ1. (1 (1 (1 (1 (1 (1 (1 (1 (1 0)))))))))))) : (∀*. (∀(∀0. 1). (∀1. 2)))
```

## TODO

- Add better AST conversion and type errors.
