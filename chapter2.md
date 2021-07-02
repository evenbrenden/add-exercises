# Chapter 2

## 2.1

> **Exercise** Prove `flipH . cw^{2*n} . flipH = cw^{2*n}`, where the ^ operation means repeated composition. For example, `cw^4 = cw . cw . cw . cw`.

Proof by induction.

`n = 1` (base case):

```
flipH . cw . cw . flipH = cw . cw
```

`n = k` (induction step):

```
flipH . cw^{2*k} . flipH = cw^{2*k}
```

`n = k + 1`:

```
flipH . cw^{2*(k + 1)} . flipH
= // Expand
flipH . cw^{2*k + 2} . flipH
= // Pull out cw^2
flipH . cw^{2*k} . cw . cw . flipH
= // Use induction step
flipH . flipH . cw^{2*k} . flipH . cw . cw . flipH
= // flipH . flipH = id
cw^{2*k} . flipH . cw . cw . flipH
= // Use base case
cw^{2*k} . cw . cw
= // Pull in cw^2
cw^{2*k + 2}
= // Factor
cw^{2*(k + 1)}
```

> **Exercise** Find a way of recreating figure 2.11, using only `cw`, `ccw` and `flipH`.

```
ccw . flipH . cw
```

> **Exercise** Derive the fact that `flipV` is its own inverse, using any of the *other* laws we've given for our algebra.

```
flipV . flipV
= // flipV = ccw . flipH . cw
flipV . ccw . flipH . cw
= // flipV = ccw . flipH . cw
ccw . flipH . cw . ccw . flipH . cw
= // cw . cww = id
ccw . flipH . flipH . cw
= // flipH . flipH = id
ccw . cw
= // cw . cww = id
id
```

> **Exercise** Derive a proof that `flipV . flipH = cw . cw`.

```
flipV . flipH
= // flipV = ccw . flipH . cw
ccw . flipH . cw . flipH
= // ccw = cw . cw . cw
cw . cw . cw . flipH . cw . flipH
= // flipH . cw = ccw . flipH
cw . cw . cw . ccw . flipH . flipH
= // cw . ccw = id
cw . cw . flipH . flipH
= // flip . flipH = id
cw . cw
```

## 2.2

> **Exercise** Prove `flipH (flipH (beside t1 t2)) = beside t1 t2` in two separate ways.

Way #1:

```
flipH (flipH (beside t1 t2))
= // flipH (flipH t) = t
beside t1 t2
```

Way #2:

```
flipH (flipH (beside t1 t2))
= // flipH (beside t1 t2) = beside (flipH t2) (flipH t1)
flipH (beside (flipH t2) (flipH t1))
= // flipH (beside t1 t2) = beside (flipH t2) (flipH t1
beside (flipH (flipH t1)) (flipH (flipH (t2))
= // flipH (flipH t) = t
beside t2 t1
```

> **Exercise** Recreate figure 2.15, using `beside`, `cw` and `ccw`.

```
cw (beside (ccw t1) (ccw t2))
```

## 2.3

> **Exercise** Give a specification for the observation of `ccw`.

**Law: "rasterize/ccw"**

```
∀ (t ::Tile) (w ::Int) (h ::Int).
  rasterize w h (ccw t) =
    reverse (transpose (rasterize h w t))
```
