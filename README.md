
# TTyped

TTyped is a dependently typed language based on the calculus of constructions.
It converts curry variables are converted to de Bruijn indices for variable
identification.

There is also a version based on intuitionistic type theory in the branch
`martin_lof`. A video describing this project can be found at
https://www.youtube.com/watch?v=_QosUn9q9fQ.

## Syntax

Comment lines start with a `#` and extend to the end of the line. Underscore
(`_`) denote an ignored variable.

### EBNF

The grammar for the core language is

```
term = forall , argAndBody
     | lambda , argAndBody
     | explicit , ( { "->" , explicit } | { explicit } ) ;

(* Here explicit refers to the explicit use of parenthesis. *)
explicit = '*'
         | sym
         | '(' , term , ')'

argAndBody = ( "_" | sym ) , ':' , explicit , '.' , term ;

forall = '∀' | '@' ;
lambda = 'λ' | '\' ;

sym = char , { char } ;
char = ? [a-zA-Z] ? ;
```

#### Binding Extensions

The core language is extended with bindings at the top level of the REPL and
files. These bindings have the form

```
repl = bind | object ;

bind = "let" , sym , '=' , object ;
```

also `sym` becomes

```
sym = ( char , { char } ) - "let" ;
```

The `let` keyword is necessary to avoid backtracing in the parser and ambiguity
with function application.

## Semantics

See "The Calculus of Constructions" by Coquand and Huet for the full semantics.
One interesting thing to note is that the syntax given in the paper rules out
unbound variables and forces `*` to be used only at the type level, while TTyped
uses separate semantics checks. This is partly done to make parsing easier, as
well as because TTyped uses Church-style variables instead of de Bruijn indices
in its syntax. These Church-style variables are then converted into de Bruijn
indices for type checking and reduction.

## Example Session

```
> stack build
> stack exec ttyped

λ> let id = \a : *. \x : a. x
λ> id
Value: λa : *. λx : a. x
Type: ∀a : *. ∀x : a. a
λ> let idT = @a : *. a -> a
λ> id idT id
Value: λa : *. λx : a. x
Type: ∀a : *. a -> a
```

Note that internally TTyped uses de Bruijn indices. Thus, there may be times
when a variable name could refer to two **different** variables. For
disambiguation, a variable name `v` that refers to a binding other than the
closest binding is also given with its de Bruijn index `n`, using the notation
`v[n]`. An example would be

```
λ> \a : *. \a : a. a
Value: λa : *. λa : a. a
Type: ∀a : *. ∀a : a. a[1]
```

Here the value the trailing `a` refers to the second variable, while in the type
`a[1]` refers to the first variable.

Files that contain bindings can also be preloaded.

```
> stack exec ttyped lib/base.tt lib/nat.tt

λ> id
Value: λa : *. λx : a. x
Type: ∀a : *. ∀x : a. a
λ> const
Value: λa : *. λx : a. λb : *. λ_ : b. x
Type: ∀a : *. ∀x : a. ∀b : *. b -> a
λ> add two three
Value: λr : *. λf : (r -> r). λx : r. f (f (f (f (f x))))
Type: ∀r : *. (r -> r) -> r -> r
λ> five
Value: λr : *. λf : (r -> r). λx : r. f (f (f (f (f x))))
Type: ∀r : *. ∀f : (r -> r). ∀x : r. r
λ> mult three three
Value: λr : *. λf : (r -> r). λx : r. f (f (f (f (f (f (f (f (f x))))))))
Type: ∀r : *. (r -> r) -> r -> r
λ> nine
Value: λr : *. λf : (r -> r). λx : r. f (f (f (f (f (f (f (f (f x))))))))
Type: ∀r : *. ∀f : (r -> r). ∀x : r. r
λ> nat
Value: ∀r : *. (r -> r) -> r -> r
```

## Notes

### The `ret` Pattern

The `ret` pattern is a technique where we assert that an object has a certain
type. Using `ret`, defined in lib/base.tt as `\a : *. \x : a. x`, we can
write `ret A x` to assert that `x : A` when type checking. This is useful to
make sure a function has the right return type. For example, `add` in lib/nat.tt
is defined as `\n : natT. \m : natT. ret natT (...)`. This pattern is used
throughout the definitions in lib. It is also useful when using the REPL to
determine the type of expressions.

## Testing

The best way to check TTyped is to run it over all the libraries. One may also
use `stack check`. However, many bugs may not be caught with the default number
of QuickCheck samples, so increasing the sample rate may be a good idea. An
example

```
> stack test --ta -a10000

ttyped-0.1.0.0: test (suite: spec, args: -a10000)


Tests.Check
  checkTerm
    reduction of type checked term produces normal form
    \a : *. \x : a. x has type @a : *. @x : a. a
Tests.Reduce
  reduceTerm
    reducing twice produces the same result
    reduction preserves types

Finished in 6.0064 seconds
4 examples, 0 failures
```

## TODO

- The pretty printer is kinda dumb when it comes to the short hand for function
  types (ie. `a -> b`). It will only print pi types this way if they ignore
  their first argument (ie. have the form `@_ : a. b`). Functions like `\x : a.
  x` have the infered type of `@x : a. a`, and thus will not have types printed
  as `a -> a`. This should be fixed.

- Add better AST conversion and type errors.

## License

Copyright (C) 2018, Taran Lynn <<taranlynn0@gmail.com>>

This program is licensed under the GPLv3 (see the LICENSE file).
