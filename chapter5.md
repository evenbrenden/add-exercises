# Chapter 5

## 5.3

> **Exercise** Prove that **"ap/pure/id"** is satisfied by the definitions of `pure` and `(<*>)` given here.

**Law: "ap/pure/id"**

```
∀ (x :: pa).
  pure id <*> pa = pa
```

```
pure id <*> pa
= // Write out pure
(\_ -> a) id <*> pa
= // Apply id to (\_ -> a)
(\p a -> a) <*> pa
= // Write out <*>
\p -> ((\p a -> a) p) (pa p)
= // Apply p to (\p a -> a)
\p -> (a -> a) (pa p)
= // (a -> a) is id
\p -> id (pa p)
= // Apply (pa p) to id
\p -> pa p
= // Make it point-free
pa
```

> **Exercise** Give **"sample/above (top)"** and **"sample/above (bottom)"**.


**Law: "sample/above (top)"**

```
∀ (t1 :: Tile a) (t2 :: Tile a) (x :: Double) (y :: Double).
  y < 0 =>
    sample (x, y) (above t1 t2) =
      sample (x, (y + 0.5) * 2) t1
```

**Law: "sample/above (bottom)"**

```
∀ (t1 :: Tile a) (t2 :: Tile a) (x :: Double) (y :: Double).
  y >= 0 =>
    sample (x, y) (above t1 t2) =
      sample (x, (y - 0.5) * 2) t2
```

> **Exercise** Implement the remaining constructors of our algebra.

```haskell
{-# LANGUAGE DerivingVia #-}

import Control.Applicative
import Data.Functor.Compose

newtype Tile a = Tile
  { sample :: Double -> Double -> a
  }
  deriving (Functor, Applicative)
    via (Compose((->) Double) ((->) Double))

cw :: Tile a -> Tile a
cw t = Tile $ \x y -> sample t y (negate x)

ccw   :: Tile a -> Tile a
ccw t = Tile $ \x y -> sample t (negate y) x

flipH :: Tile a -> Tile a
flipH t = Tile $ \x y -> sample t (negate x) y

flipV :: Tile a -> Tile a
flipV t = Tile $ \x y -> sample t x (negate y)

quad  :: Tile a -> Tile a -> Tile a -> Tile a -> Tile a
quad t1 t2 t3 t4 = above (beside t1 t2) (beside t3 t4)

swirl :: Tile a -> Tile a
swirl t = quad t (cw t) (ccw t) ((cw . cw) t)

beside :: Tile a -> Tile a -> Tile a
beside t1 t2 = Tile $ \x y ->
  if x < 0
    then sample t1 ((x + 0.5) * 2) y
    else sample t2 ((x - 0.5) * 2) y

above  :: Tile a -> Tile a -> Tile a
above t1 t2 = Tile $ \x y ->
  if y < 0
    then sample t1 x ((y + 0.5) * 2)
    else sample t2 x ((y - 0.5) * 2)

empty  :: Monoid a => Tile a
empty = pure mempty

behind :: Monoid a => Tile a -> Tile a -> Tile a
behind = flip $ liftA2 (<>)
```
