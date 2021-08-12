#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.generic-data p.monoid-subclasses p.monoidal-containers])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

import Control.Monad
import Data.Map.Monoidal (MonoidalMap, singleton, toList)
import Data.Semigroup
import Data.Semigroup.Cancellative
import Data.Set (fromList)
import Generic.Data

-- ClueState implementation

data ClueState
  = Seen | Failed | Completed
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (Semigroup, Monoid) via Max ClueState

seen :: ClueState
seen = Seen

completed :: ClueState
completed = Completed

failed :: ClueState
failed = Failed

-- InputFilter implementation

class HasFilter i where
  data CustomFilter i
  filterMatches :: CustomFilter i -> i -> Bool

data InputFilter i
  = Always
  | Never
  | And (InputFilter i) (InputFilter i)
  | Or (InputFilter i) (InputFilter i)
  | Not (InputFilter i)
  | Custom (CustomFilter i)
  deriving stock (Generic)

deriving stock instance (Eq (CustomFilter i)) => Eq (InputFilter i)
deriving stock instance (Ord (CustomFilter i)) => Ord (InputFilter i)
deriving stock instance (Show (CustomFilter i)) => Show (InputFilter i)

_always :: InputFilter i
_always = Always

never  :: InputFilter i
never = Never

_andF :: InputFilter i -> InputFilter i -> InputFilter i
_andF = And

_orF  :: InputFilter i -> InputFilter i -> InputFilter i
_orF = Or

_notF :: InputFilter i -> InputFilter i
_notF = Not

custom :: CustomFilter i -> InputFilter i
custom = Custom

matches :: HasFilter i => InputFilter i -> i -> Bool
matches Always       _ = True
matches Never        _ = False
matches (And f1 f2)  i = matches f1 i && matches f2 i
matches (Or f1 f2)   i = matches f1 i || matches f2 i
matches (Not f)      i = not $ matches f i
matches (Custom f) i = filterMatches f i

-- Results implementation

data Results k r = Results
  { rewards :: r
  , clues   :: MonoidalMap [k] ClueState
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Semigroup, Monoid)
    via Generically (Results k r)

instance (Show k, Show r) => Show (Results k r) where
  show (Results r k) = mconcat
    [ "Results ("
    , show r
    , ") (fromList "
    , show $ toList k
    , ")"
    ]

-- Challenge implementation

data Challenge i k r
  = Empty
  | Gate (InputFilter i) (Challenge i k r)
  | Clue       k (Challenge i k r)
  | RewardThen r (Challenge i k r)
  | EitherC (Challenge i k r) (Challenge i k r)
  | Both    (Challenge i k r) (Challenge i k r)
  | AndThen (Challenge i k r) (Challenge i k r)
  deriving stock (Generic)

deriving stock instance
  (Eq r, Eq k, Eq (CustomFilter i))
    => Eq (Challenge i k r)

deriving stock instance
  (Show r, Show k, Show (CustomFilter i))
    => Show (Challenge i k r)

pumpChallenge
    :: forall i k r
     . ( Ord k
       , HasFilter i
       , Monoid r, Commutative r, Eq r
       )
    => Challenge i k r
    -> [i]
    -> (Results k r, Challenge i k r)
pumpChallenge c
  = foldM (flip $ step []) c
  . (Nothing :)
  . fmap Just

_runChallenge
    :: forall i k r.
      ( HasFilter i, Eq (CustomFilter i)
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> [i]
    -> (Results k r, Bool)
_runChallenge c = fmap (== Empty) . pumpChallenge c

_isEmpty
    :: forall i k r.
      ( HasFilter i, Eq (CustomFilter i)
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> Bool
_isEmpty = (== Empty) . snd . flip pumpChallenge []

step
    :: forall i k r
     . ( HasFilter i
       , Ord k
       , Monoid r, Commutative r, Eq r
       )
    => [k]
    -> Maybe i
    -> Challenge i k r
    -> (Results k r, Challenge i k r)
step _ _ Empty = pure empty

step kctx i (Both c1 c2)
  = both <$> step kctx i c1 <*> step kctx i c2

step kctx i (EitherC c1 c2) = do
  c1' <- step kctx i c1
  c2' <- step kctx i c2
  case (c1', c2') of
    (Empty, _) -> prune kctx c2'
    (_, Empty) -> prune kctx c1'
    _          -> pure $ eitherC c1' c2'

step kctx i (AndThen c1 c2) =
  step kctx i c1 >>= \case
    Empty -> step kctx Nothing c2
    c1' -> pure $ andThen c1' c2

step kctx i (RewardThen r c) =
  let r' = Results r mempty
      (r'', c') = step kctx i c
  in (r' <> r'', c')

step kctx (Just i) (Gate f c)
  | matches f i = step kctx Nothing c
step _ _ c@Gate{} = pure c

step kctx i (Clue k c) = do
  let kctx' = kctx <> [k]
  step kctx' i c >>= \case
    Empty ->
      let r = Results mempty $ singleton kctx' completed
      in (r, Empty)
    c' ->
      let r = Results mempty $ singleton kctx' seen
      in (r, clue [k] c')

prune
    :: (Ord k, Monoid r)
    => [k]
    -> Challenge i k r
    -> (Results k r, Challenge i k r)
prune kctx c =
  let k' = fmap (<> failed) $ findClues kctx c
      r' = Results mempty k'
      (r'', c') = pure empty
  in (r' <> r'', c')

findClues
    :: forall i k r
     . Ord k
    => [k]
    -> Challenge i k r
    -> MonoidalMap [k] ClueState
findClues _    Empty
  = mempty
findClues kctx (Both c1 c2)
  = findClues kctx c1 <> findClues kctx c2
findClues kctx (EitherC c1 c2)
  = findClues kctx c1 <> findClues kctx c2
findClues _    (Gate _ _)
  = mempty
findClues kctx (AndThen c _)
  = findClues kctx c
findClues kctx (RewardThen _ c)
  = findClues kctx c
findClues kctx (Clue k Empty)
  = singleton (kctx <> [k]) completed
findClues kctx (Clue k c)
  = singleton (kctx <> [k]) seen
    <> findClues (kctx <> [k]) c

rewardThen
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => r -> Challenge i k r -> Challenge i k r
rewardThen r c | r == mempty = c
rewardThen r' (RewardThen r c) = RewardThen (r <> r') c
rewardThen r c = RewardThen r c

-- Public interface

empty :: forall i k r. Challenge i k r
empty = Empty

_bottom :: forall i k r. Challenge i k r
_bottom = gate never empty

clue
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => [k] -> Challenge i k r -> Challenge i k r
clue [] c = c
clue k (RewardThen r c) = rewardThen r (clue k c)
clue k c = foldr Clue c k

reward
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => r -> Challenge i k r
reward r = rewardThen r empty

gate
    :: forall i k r
     . InputFilter i
    -> Challenge i k r
    -> Challenge i k r
gate = Gate

both
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
     => Challenge i k r
     -> Challenge i k r
     -> Challenge i k r
both (RewardThen r c1) c2 = rewardThen r (both c1 c2)
both c1 (RewardThen r c2) = rewardThen r (both c1 c2)
both Empty c2 = c2
both c1 Empty = c1
both c1 c2 = Both c1 c2

andThen
    :: forall i k r
     . ( Monoid r, Commutative r, Eq r
       )
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
andThen Empty c = c
andThen (Gate f c1) c2 = gate f (andThen c1 c2)
andThen (RewardThen r c1) c2 =
  rewardThen r (andThen c1 c2)
andThen (AndThen c1 c2) c3 =
  andThen c1 (andThen c2 c3)
andThen c1 c2 = AndThen c1 c2

eitherC
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
eitherC (RewardThen r c1) c2 =
  rewardThen r (eitherC c1 c2)
eitherC c1 (RewardThen r c2) =
  rewardThen r (eitherC c1 c2)
eitherC c1 c2 = EitherC c1 c2

getRewards
    :: forall i k r.
      ( HasFilter i
      , Ord k
      , Monoid r, Commutative r, Eq r
      ) =>
      Challenge i k r -> [i] -> r
getRewards c = rewards . fst . pumpChallenge c

getClues
    :: forall i k r.
      ( HasFilter i
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> [i]
    -> MonoidalMap [k] ClueState
getClues c = clues . fst . pumpChallenge c

-- Give it a spin

type Point = (Int, Int)

instance HasFilter Point where
  data CustomFilter Point = PointOfInterest Point
    deriving stock (Eq, Ord, Show, Generic)
  filterMatches (PointOfInterest (poiX, poiY)) (x, y) =
    x == poiX && y == poiY

main :: IO ()
main = do
  let clueFilter = custom $ PointOfInterest (10, 10)
  let rewardFilter = custom $ PointOfInterest (0, 0)
  let cluesIn = ["Origo"]
  let rewardsIn = fromList ["The journey is the goal"]
  let challenge = andThen
                    (gate clueFilter (clue cluesIn empty))
                    (gate rewardFilter (reward rewardsIn))
  let inputs = [(10, 10), (5, 5), (0, 0)]
  let cluesOut = getClues challenge inputs
  let rewardsOut = getRewards challenge inputs
  print cluesOut
  print rewardsOut
  return ()
