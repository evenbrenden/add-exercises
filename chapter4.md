# Chapter 4

> **Exercise** Attempt a quick design of this system before continuing. Sketch the core data types and functions necessary for a scavenger hunt system like described above.  How would you encode a challenge like "take a selfie at the library, and receive one point afterward."

```haskell
newtype Clue = Clue String

data Location = Location (Int, Int)

data Radius = Radius Int

data Path = Path [Location]

data Photo a = Photo a

data Snap a = Snap
    { photo :: Photo a
    , geoTag :: Location
    }

newtype Reward a = Reward a

data Challenge a = Challenge
    { clue :: Clue
    , location :: Location
    , radius :: Radius
    , reward :: Reward a
    }

track :: Path -> Location -> Path
track = undefined

check :: Location -> Radius -> Location -> Bool
check = undefined

snap :: Challenge a -> Snap b -> Maybe (Reward a)
snap = undefined

libraryChallenge :: Challenge Int
libraryChallenge = Challenge
    { clue = Clue "Take a selfie at the library, and receive one point afterward."
    , location = Location (100, 100)
    , radius = Radius 2
    , reward = Reward 1
    }
```

> **Exercise** Try to represent a challenge that requires players to go to the beach and to the library — but in either order — and then receive a reward. Can your initial design represent this problem? If not, modify the core data types to support this functionality.

I would need to make the following changes and additions:

```haskell
data Challenge a = Challenge
    { clue :: Clue
    , locations :: [Location]
    , radius :: Radius
    , reward :: Reward a
    }

snaps :: Challenge a -> [Snap b] -> Maybe (Reward a)
snaps = undefined

beachLibraryChallenge :: Challenge Int
beachLibraryChallenge = Challenge
    { clue = Clue "Take a selfie at the beach and at the library, and receive a reward."
    , locations = [Location (100, 100), Location (200, 200)]
    , radius = Radius 2
    , reward = Reward 1
    }
```

> **Exercise** What implications does your design have, in terms of its eventual usability, performance, complexity, and extensibility?

Eventual usability: Data constructors are hopefully at the right level of abstraction for a non-technical user, but the types that they wrap (lists of tuples of `Int`s, generic `a`s, etc.) are not.

Performance: Proportional to the number of locations.

Complexity: Given the uses cases thus far, an *ordered* list is not necessary.

Extensibility: Reward may be extended to e.g. multiple awards though its generic `a`, but that's about it.

> **Exercise** Reduce `getRewards (clue c (reward r)) is` to its simplest form via algebraic manipulation.

```
getRewards (clue c (reward r)) is
= // getRewards (clue k c) = getRewards c
getRewards (reward r) is
= // getRewards (reward r) is = [r]
[reward r]
```

## 4.1

> **Exercise** Give semantics in terms of `matches` for both `photoWithin` and `photoAbove`. Remember that these are now functions over `InputFilter`, not over `Challenge` like before. Does the new type simplify the semantics?

SKIPPED

## 4.2

> **Exercise** Consider the term `gate (photoWithin p1 d1) (gate (photoWithin p2 d2) (reward r))`. What is your intuitive understanding of this expression? Do our stated semantics agree with you? Hint: try evaluating `getRewards` of the above, using two different photo inputs.

```haskell
photoWithin :: Point -> Distance -> InputFilter

gate :: InputFilter -> Challenge -> Challenge

reward :: Reward -> Challenge

getRewards :: Challenge -> [Input] -> [Reward]

temp = gate (photoWithin p2 d2) (reward r) :: Challenge

term = gate (photoWithin p1 d1) temp :: Challenge

getRewards term :: [Input] -> [Reward]
```

My intuitive understanding is that in order to collect the reward `r`, you first need to pass through the gate pertaining to `p1` and `d1`, then the gate pertaining to `p2` and `d2`. Applying the term to `getReward` gives a function that, when fed a list of inputs that contains the right inputs in the right order, returns `[r]`. Otherwise, it returns `[]`. It does not matter what else is in the input list. In other words, you can make as many errors as you like as long as the order of the successes is right. You may also revisit a previously passed gate at any point in time.

> **Exercise** Use `locWithin` to encode a challenge that requires our player to walk around the block twice, clockwise, as in figure 4.9. Assume you have `p1, p2, p3 :: Point` and `d1, d2, d3 :: Distance` corresponding to each corner's locations and tolerances.

```haskell
doubleClockwiseWalkAroundTheBlock =
  let gate1 = gate (locWithin p1 d1)
      gate2 = gate (locWithin p2 d2)
      gate3 = gate (locWithin p3 d3)
  in  gate1 (gate 2 (gate3 (gate1 (gate2 (gate3 (reward r))))))
```

> **Exercise** Prove that `both (both c1 c2) (both c3 c4) = both c1 (both c2 (both c3 c4))`. How might an implementation use this fact?

```
both c1 (both c2 (both c3 c4))
= // Associativity
both (both c1 c2) (both c3 c4)
```

> **Exercise** There is only one "reasonable" semantics for threading inputs through `both` with respect to `getRewards`. What is it?

**Law: "getRewards/both"**

```
∀ (c1 :: Challenge) (c2 :: Challenge) (is :: [Input]).
  getRewards (both c1 c2) is
    = getRewards c1 is <> getRewards c2 is
```

## 4.3

> **Exercise** Give semantics for `andThen` in terms of `completes :: Challenge -> [Input] -> Bool`. Show that your these semantics necessarily contradict **"andThen/gate"**.

SKIPPED

> **Exercise** Give a law for the behavior of `shorterOf`.

**Law: "shorterOf"**

```
∀ (l1 :: [a]) (l2 :: [a]).
  length l1 < l2 =>
    shorterOf l1 l2 = l1
```

> **Exercise** Does `shorterOf` form a monoid? If so, give it. If not, show which laws it doesn't satisfy.

**Law: "shorterOf:associative"**

```
∀ (l1 :: [a]) (l2 :: [a]) (l3 :: [a]).
  shorterOf l1 (shorterOf l2 l3) = shorterOf (shorterOf l1 l2) l3
```

**Law: "shorterOf:identity"**

```
∀ (l :: [a]).
  shorterOf l [] = shorterOf [] l
```

# 4.6

> **Exercise** Does `eitherC` form a monoid? If so, what is its `identity` element?

Just like the identity for the boolean OR monoid is FALSE, we need a constructor for a challenge that can never be completed:

```haskell
unfinishable :: Challenge
```

**Law: "eitherC:identity"**

```
∀ (c1 :: Challenge).
  eitherC c unfinishable = eitherC unfinishable c
```

> **Exercise** Reduce `eitherC (reward r1) empty` to its simplest form.

```
eitherC (reward r1) empty
= // reward r = andThen (reward r) empty
eitherC (andThen (reward r) empty) empty
= // Distributivity
andThen (reward r) (eitherC empty empty)
= // Short-circuiting
andThen (reward r) empty
= // andThen (reward r) empty = reward r
reward r
```

> **Exercise** Encapsulate this timeout behavior in a new `timeout` constructor.  Be sure to give it a type and sufficient laws to entirely specify its behavior.

```haskell
timeout :: Time -> Bool
```

**Law: "getRewards/afterTime not timed out"**

```
∀ (t :: Time) (i :: Input).
  not (timeout t) =>
    step (Just i) (gate (afterTime t) empty)
      = pure step Nothing empty
```

**Law: "getRewards/afterTime timed out"**

```
∀ (t :: Time) (i :: Input).
  timeout t =>
    step (Just i) (gate (afterTime t) empty)
      = pure (gate (afterTime t) empty)
```
