
# TTyped

TTyped is a dependently typed language based on the Calculus of
Constructions (CoC). Features/quirks include

1. Conversion from curry variables to de Bruijn indices for variable
   identification.
2. The ability to define axioms.
3. Full reduction of terms that don't contain axioms.
4. Extraction to the Untyped Lambda Calculus and Scheme.
5. A REPL that allows for easy experimentation with the CoC.

There is also a version based on intuitionistic type theory in the
branch `martin_lof` (**obsolete**). A video describing this project
can be found at https://www.youtube.com/watch?v=_QosUn9q9fQ.

## Syntax

Comment lines start with a `#` and extend to the end of the line. Underscores
(`_`) denote ignored variables.

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
repl = axiom | bind | object ;

axiom = "axiom" , sym , ':' , term ;

bind = "let" , sym , '=' , object ;
```

also `sym` becomes

```
sym = ( char , { char } ) - ("axiom" | "let");
```

The `let` keyword is necessary to avoid backtracing in the parser and ambiguity
with function application.

## Semantics

TTyped's semantics is essentially the same as the CoC extended with
axioms. See "The Calculus of Constructions" by Coquand and Huet for
its full semantics. One interesting thing to note is that the syntax
given in the paper rules out unbound variables and forces `*` to be
used only at the type level, while TTyped uses separate semantics
checks. This is partly done to make parsing easier, as well as because
TTyped uses Church-style variables instead of de Bruijn indices in its
syntax. These Church-style variables are then converted into de Bruijn
indices for type checking and reduction.

### Axioms

Axioms is TTyped's extension to the CoC. Axioms are symbolic that take
on any valid variable type. One may think of an axiom `ax` of type `t`
as a wrapper around the whole program like so `\ax : t. ...`.

**TODO**: Give typing judgements and reductions for axioms. They
should be almost identical to those for variables.

## Example Session

```
> rlwrap stack run

λ> let id = \a : *. \x : a. x
λ> id
Value     | λa : *. λx : a. x
Type      | ∀a : *. ∀x : a. a
Extracted | λx1. x1
λ> let idT = @a : *. a -> a
λ> idT
Value     | ∀a : *. a -> a
Type      | *
λ> id idT id
Value     | λa : *. λx : a. x
Type      | ∀a : *. a -> a
Extracted | λx1. x1
λ> axiom bot : @a : *. a
λ> bot idT
Value     | bot (∀a : *. a -> a)
Type      | ∀a : *. a -> a
Extracted | bot
```

A couple of things should be readily apparent from the above
example. First, all non-let expression will have their value and type
printed. Second, any value that is not a type will be extracted to the
untyped lambda calculus and the result will be printed. Third, axiom
applications will, and cannot, be reduced.

Note that internally TTyped uses de Bruijn indices. Thus, there may be times
when a variable name could refer to two **different** variables. For
disambiguation, a variable name `v` that refers to a binding other than the
closest binding is also given with its de Bruijn index `n`, using the notation
`v[n]`. If `v` is an axiom, then `∞` is used, giving `v[∞]`. An example would be

```
λ> \a : *. \a : a. a
Value     | λa : *. λa : a. a
Type      | ∀a : *. ∀a : a. a[1]
Extracted | λa1. a1
```

Here the value the trailing `a` refers to the second variable, while in the type
`a[1]` refers to the first variable.

Files that contain bindings can also be preloaded.

```
> rlwrap stack run lib/base.tt lib/nat.tt

λ> id
Value     | λa : *. λx : a. x
Type      | ∀a : *. ∀x : a. a
Extracted | λx1. x1
λ> const
Value     | λa : *. λx : a. λb : *. λ_ : b. x
Type      | ∀a : *. ∀x : a. ∀b : *. b -> a
Extracted | λx1. λv3. x1
λ> add two three
Value     | λr : *. λf : (r -> r). λx : r. f (f (f (f (f x))))
Type      | ∀r : *. (r -> r) -> r -> r
Extracted | λf1. λx2. f1 (f1 (f1 (f1 (f1 x2))))
λ> five
Value     | λr : *. λf : (r -> r). λx : r. f (f (f (f (f x))))
Type      | ∀r : *. ∀f : (r -> r). ∀x : r. r
Extracted | λf1. λx2. f1 (f1 (f1 (f1 (f1 x2))))
λ> mult three three
Value     | λr : *. λf : (r -> r). λx : r. f (f (f (f (f (f (f (f (f x))))))))
Type      | ∀r : *. (r -> r) -> r -> r
Extracted | λf1. λx2. f1 (f1 (f1 (f1 (f1 (f1 (f1 (f1 (f1 x2))))))))
λ> nine
Value     | λr : *. λf : (r -> r). λx : r. f (f (f (f (f (f (f (f (f x))))))))
Type      | ∀r : *. ∀f : (r -> r). ∀x : r. r
Extracted | λf1. λx2. f1 (f1 (f1 (f1 (f1 (f1 (f1 (f1 (f1 x2))))))))
λ> nat
Value     | ∀r : *. (r -> r) -> r -> r
Type      | *
```


## Scheme Extraction

Scheme code can be extracted with the `--extract` flag. This option
must be given a directory to output extracted Scheme scripts to. To
run the example `hello` application run.

```
> mkdir tmp
> stack run -- lib/base.tt lib/scheme/base.tt examples/hello.tt -e tmp
> cat tmp/lib_base.tt.scm lib/scheme/base.scm  tmp/lib_scheme_base.tt.scm tmp/examples_hello.tt.scm examples/hello.scm > tmp/out.scm
```

You can now run `tmp/out.scm` using an interpreter of your choice. The
script should simply print `5`.

### Axioms

Axioms are left unimplemented in the extracted Scheme code, and must
be implemented in order for the extracted code to run. This can be
used to implement a FFI. See `examples/` and `lib/scheme/` for
examples.


## Notes

### Use of Axioms

tl;dw **DO NOT** use axioms if you are doing proofs!!!

The axiom statement allows us to introduces terms whose types are not
constructible within the confines of CoC. Since this also allows any type
to be inhabited, any program that uses axioms may be unsound (see
`lib/fix.tt), and as such it is recommended to avoid them when doing
proofs. However, axioms are useful for several things.

- We want to introduce logical axioms which are impossible under CoC
  (see `lib/classical.tt`).
- As an FFI to Scheme.

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

## Contributing

If you wish to contribute please open up a GitHub issue or PR. I want
to keep TTyped as a minimal implementation of the CoC, so extensions
to the core language semantics will probably be turned down. However,
syntax extensions that improve QoL would be nice. Also, any help in
finding bugs in the type checker and reduction engine would be much
appreciated.

## TODO

- The pretty printer is kinda dumb when it comes to the short hand for function
  types (ie. `a -> b`). It will only print pi types this way if they ignore
  their first argument (ie. have the form `@_ : a. b`). Functions like `\x : a.
  x` have the infered type of `@x : a. a`, and thus will not have types printed
  as `a -> a`. This should be fixed.

- Add better AST conversion and type errors.

## License

Copyright (C) 2020, Taran Lynn <<taranlynn0@gmail.com>>

This program is licensed under the GPLv3 (see the LICENSE file).
