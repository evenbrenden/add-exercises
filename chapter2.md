# 2.1

> **Exercise** Prove `flipH . cw^{2*n} . flipH = cw^{2*n}`, where the ^ operation means repeated composition. For example, `cw^4 = cw . cw . cw . cw.`

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
flipV = ccw . flipH . cw
```
