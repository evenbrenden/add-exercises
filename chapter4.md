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