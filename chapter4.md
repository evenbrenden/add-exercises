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

Still using `getRewards` to observe, we need a way to get from `InputFilter` to an updated `Challenge` in order to restate our law(s):

```
continue :: InputFilter -> Challenge -> Challenge
```

**Law: "getRewards/photoWithin"**

```
∀ (p :: Point) (d :: Distance) (pic :: Photo) (c ::Challenge) (is :: [Input]).
  matches (photoWithin p d) (photo p pic) =>
    getRewards (continue (photoWithin p d) c) (photo p pic : is)
      = getRewards c is
```

**Law: "getRewards/photoAbove"**

```
∀ (a :: Altitude) (pic :: Photo) (c :: Challenge) (is :: [Input]).
  matches (photoAbove a) (photo p pic) =>
    getRewards (continue (photoAbove a) c) (photo p pic : is)
      = getRewards c is
```

Got rid of `within`, but I don't think it's any simpler than before. Then again maybe it's not correct.

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
