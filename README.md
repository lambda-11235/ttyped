
# TTyped

TTyped is a dependently typed language based on the calculus of constructions.
It converts curry variables are converted to de Bruijn indices for variable
identification.

There is also a version based on intuitionistic type theory in the branch
`martin_lof`.

## Syntax

Comment lines start with a `#` and extend to the end of the line.

### EBNF

The core language that the type checker and reduce operate on has the grammar

```
term = context | object ;

context = '*'
        | '(' , forall , sym , ':' , term , '.' , context , ')' ;

object = sym
        | '(' , forall , sym , ':'  , term , '.' , object , ')' ;
        | '(' , lambda , sym , ':'  , term , '.' , object , ')' ;
        | '(' , object , object , { object } ')' ;

forall = '∀' | '@' ;
pi = 'Π' | "||" ;
lambda = 'λ' | '\' ;

sym = char , { char } ;
char = ? [a-zA-Z] ? ;
```

#### Binding Extensions

The core language is extended with bindings at the top level of the REPL and
files. These bindings have the form

```
repl = bind | object ;

bind = sym , '=' , object ;
```

## Semantics

See "The Calculus of Constructions" by Coquand and Huet. The underlying reducer
and type checker use de Bruijn indices.

## Example Session

```
> stack build
> stack exec ttyped

λ> id = (\a : *. (\x : a. x))
λ> id
Value: (λa : *. (λx : a[0]. x[0]))
Type: (∀a : *. (∀x : a[0]. a[1]))
λ> idT = (@a : *. (@x : a. a))
λ> ((id idT) id)
Value: (λa : *. (λx : a[0]. x[0]))
Type: (∀a : *. (∀x : a[0]. a[1]))
```

The printed variables are displayed in the form `v[n]` for some number `n`. This
number represents the de Bruijn index of the variable, and is displayed because
all reductions and type checking is done with de Bruijn indices. Thus, there are
case where the same variable name may have different scopes. An example is

```
λ> (\a : *. (\a : a. a))
Value: (λa : *. (λa : a[0]. a[0]))
Type: (∀a : *. (∀a : a[0]. a[1]))
```

Notice how in the type `(∀a : *. (∀a : a[0]. a[1]))` the innermost `a` refers to
the outerbound `a`, and not the inner bound `a`.

Files that contain bindings can also be preloaded.

```
> stack exec ttyped lib/base.tt lib/nat.tt

λ> id
Value: (λa : *. (λx : a[0]. x[0]))
Type: (∀a : *. (∀x : a[0]. a[1]))
λ> const
Value: (λa : *. (λx : a[0]. (λb : *. (λy : b[0]. x[2]))))
Type: (∀a : *. (∀x : a[0]. (∀b : *. (∀y : b[0]. a[3]))))
λ> (add two three)
Value: (λr : *. (λf : (∀x : r[0]. r[1]). (λx : r[1]. (f[1] (f[1] (f[1] (f[1] (f[1] x[0]))))))))
Type: (∀r : *. (∀f : (∀x : r[0]. r[1]). (∀x : r[1]. r[2])))
λ> five
Value: (λr : *. (λf : (∀x : r[0]. r[1]). (λx : r[1]. (f[1] (f[1] (f[1] (f[1] (f[1] x[0]))))))))
Type: (∀r : *. (∀f : (∀x : r[0]. r[1]). (∀x : r[1]. r[2])))
λ> (mult three three)
Value: (λr : *. (λf : (∀x : r[0]. r[1]). (λx : r[1]. (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] x[0]))))))))))))
Type: (∀r : *. (∀f : (∀x : r[0]. r[1]). (∀x : r[1]. r[2])))
λ> nine
Value: (λr : *. (λf : (∀x : r[0]. r[1]). (λx : r[1]. (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] (f[1] x[0]))))))))))))
Type: (∀r : *. (∀f : (∀x : r[0]. r[1]). (∀x : r[1]. r[2])))
λ> natType
Value: (∀r : *. (∀f : (∀x : r[0]. r[1]). (∀x : r[1]. r[2])))
Type: *
```

## Notes

### The `id` Pattern

The `id` pattern is a technique where we assert that an object has a certain
type. Using `id`, defined in lib/base.tt as `(\a : *. (\x : a. x))`, we can
write `(id A x)` to assert that `x : A` when type checking. This is useful to
make sure a function has the right return type. For example, `add` in lib/nat.tt
is defined as `(\n : natT. (\m : natT. (id natT ...)`. This pattern is used
throughout the definitions in lib. It is also useful when using the REPL to
determine the type of expressions.

## TODO

- Add better AST conversion and type errors.
