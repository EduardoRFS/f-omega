```rust
Weak A = _;
pure : (A : Type) -> A -> Weak A = _;
weak : (A : Type) -> A -> Weak Unit = _;
bind : (A : Type) -> (B : Type) -> Weak A -> (A -> Weak B) -> Weak B = _;

Pair A B = (K : Type) -> (A -> B -> K) -> K;
pair A B a b : Pair A B = K => k => k a b;

Either A B = (K : Type) -> (A -> Weak K) -> (B -> Weak K) -> Weak K;
left A B a : Either A B = K => l => r =>
  bind Unit (weak (B -> Weak K) r) (unit => unit (Weak K) (l a));
right A B b : Either A B = K => l => r =>
  bind Unit (weak (A -> Weak K) l) (unit => unit (Weak K) (r b));
```

```rust
type Unit === forall A. A -> A in
let unit : Unit === fun (type A) x -> x in

type Pair A B === forall K. (A -> B -> K) -> K in
let pair (type A) (type B) (a : A) (b : B) : Pair A B ===
  fun (type K) k -> k a b in

type Garbage === forall K. (forall A. A -> K) -> K in
let empty : Garbage === fun (type K) k -> k (type Unit) unit in
let collect : forall A. A -> Garbage -> Garbage ===
  fun (type A) x garbage ->
  fun k -> k (type Pair A Garbage) (pair (type A) (type Garbage) x garbage) in

type Weak A === Garbage -> Pair A Garbage in
let pure : forall A. A -> Weak A ===
  fun (type A) x ->
  fun garbage -> pair (type A) (type Garbage) x garbage in
let weak : forall A. A -> Weak Unit ===
  fun (type A) x ->
  fun garbage ->
    let garbage === collect (type A) x garbage in
    pair (type Unit) (type Garbage) unit garbage in
let bind : forall A B. Weak A -> (A -> Weak B) -> Weak B ===
  fun (type A) (type B) m f ->
  fun garbage ->
    let value_and_garbage === m garbage in
    value_and_garbage (fun x garbage -> f x garbage) in
let map : forall A B. Weak A -> (A -> B) -> Weak B ===
  fun (type A) (type B) m f ->
    bind (type A) (type B) m (fun x -> pure (type B) (f x)) in

type Bool === forall A. A -> A -> Weak A in
let true : Bool === fun (type A) x y ->
  bind (type Unit) (type A) (weak (type A) y)
    (unit => unit (type Weak A) (pure (type A) x)) in
let false : Bool === fun (type A) x y ->
  bind (type Unit) (type A) (weak (type A) x)
    (unit => unit (type Weak A) (pure (type A) y)) in

type Either A B === (K : Type) -> (A -> K) -> (B -> K) -> Weak K;
let left (type A) (type B) (a : A) : Either A B ===
  fun (type K) l r ->
    bind Unit (weak (B -> K) r)
      (unit => pure (type K) (unit (type K) (l a))) in
let right (type A) (type B) (a : A) : Either A B ===
  fun (type K) l r ->
    bind Unit (weak (A -> K) l)
      (unit => pure (type K) (unit (type K) (r a))) in
left
```
