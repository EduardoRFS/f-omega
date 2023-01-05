# Linear F

This is a prototype intended to test the limits of a Linear System F. This system has only a linear kind.

## Calculus

All value variables are linear. All type variables are traditional. Aliases and annotations are just sugar, so no type constructor available.

```rust
Type =
  | X
  | forall X. Type
  | Type -> Type
Expr =
  | x
  // F Lambda
  | fun (type A) -> Expr
  | Expr (type Type)
  // STLC Lambda
  | fun x -> Expr
  | Expr Expr
  // aliases
  | type A === Type in Expr
  | type X A B ... === Type in Expr
  | let x === Expr in Expr
  // annot
  | (Expr : Type)
```

## Affine F

This example shows that you can have an explicit / monadic form of weakening. Allowing to model an Affine System F. This shows that Linear F can at least describe PLUS `⊕`.

```shell
cat examples/weak.linf | dune exec check
```

## Weak DUP

While this system doesn't include contraction in the general case, most cases(maybe all?) of System F cases can be explicitly done, by duplicating at call site. Very likely a "Graded F" could be elaborate to Linear F.

But for simple dups, such as duplicating a boolean or an either, it can be done.

```shell
cat examples/dup.linf | dune exec check
```
