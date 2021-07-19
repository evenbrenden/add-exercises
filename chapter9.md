# Chapter 9

## 9.2.2

> **Exercise** Most of the semigroups we saw in chapter 9.2.1 also form monoids. For each semigroup described there, determine if it has an identity element or not.

- List concatenation: `mempty = []`.
- String concatenation: `mempty = ""`.
- Addition: `mempty = 0`.
- Multiplication: `mempty = 1`.
- Boolean AND: `mempty = True`.
- Boolean OR: `mempty = False`.
- The `behind` color mixing operation: `mempty = color 0 0 0 0`.
- The `min` function: Generally for `Ord`, no. For `Bounded` instances, `mempty = maxBound`.
- The `max` function: Generally for `Ord`, no. For `Bounded` instances, `mempty = minBound`.
- Picking the first and last element in a series: The empty series (e.g. `[]`), if it exists, is the identity element. If the empty series does not exist (as with the `NonEmpty` list), this operation does not form a monoid.
- Function composition: `mempty = id`.

## 9.2.3

> **Exercise** Prove the corollary that `invert mempty = mempty`.

```
invert mempty
= // Identity
invert mempty <> mempty
= // Invertibility
mempty
```

## 9.2.6

> **Exercise** Show that both `([()], cartesianProduct)` and `(repeat (), zip)` form applicative functors over lists.

**"fmap/fst/zap"** for `([()], cartesianProduct)`:

```
fmap fst zap x unit
=
fmap fst cartesianProduct x [()]
=
fmap fst [(x1, ()), (x2, ()), ...]
=
[x1, x2, ...]
=
x
```

**"fmap/snd/zap"** for `([()], cartesianProduct)`:

```
fmap snd zap unit x
=
fmap snd cartesianProduct [()] x
=
fmap snd [((), x1), ((), x2), ...]
=
[x1, x2, ...]
=
x
```

**"fmap/reassoc/zap"** for `([()], cartesianProduct)`:

```
fmap reassoc (zap x (zap y z))
=
fmap reassoc (cartesianProduct x (cartesianProduct y z))
=
fmap reassoc [(x1, (y1, z1)), (x2, (y2, z2)), ...]
=
[((x1, y1), z1), ((x2, y2), z2), ...]
=
cartesianProduct (cartesianProduct x y) z
=
zap (zap x y) z
```

**"fmap/fst/zap"** for `(repeat (), zip)`:

```
fmap fst zap x unit
=
fmap fst zip x (repeat ())
=
fmap fst [(x1, ()), (x2, ()), ...]
=
[x1, x2, ...]
=
x
```

**"fmap/snd/zap"** for `(repeat (), zip)`:

```
fmap snd zap unit x
=
fmap snd zip (repeat ()) x
=
fmap snd [((), x1), ((), x2), ...]
=
[x1, x2, ...]
=
x
```

**"fmap/reassoc/zap"** for `(repeat (), zip)`:

```
fmap reassoc (zap x (zap y z))
=
fmap reassoc (zip x (zip y z))
=
fmap reassoc [(x1, (y1, z1)), (x2, (y2, z2)), ...]
=
[((x1, y1), z1), ((x2, y2), z2), ...]
=
zip (zip x y) z
=
zap (zap x y) z
```
