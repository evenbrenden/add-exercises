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
import Control.Monad.Writer.Class
import Data.Map.Monoidal (MonoidalMap, singleton, toList)
import Data.Semigroup
import Data.Semigroup.Cancellative
import Data.Set (Set, fromList)
import Generic.Data

-- Scavanger Hunt implementation

data ClueState
  = Seen | Failed | Completed
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (Semigroup, Monoid) via Max ClueState

_seen :: ClueState
_seen = _seen

_completed :: ClueState
_completed = Completed

_failed :: ClueState
_failed = Failed

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

_never  :: InputFilter i
_never = Never

_andF :: InputFilter i -> InputFilter i -> InputFilter i
_andF = And

_orF  :: InputFilter i -> InputFilter i -> InputFilter i
_orF = Or

_notF :: InputFilter i -> InputFilter i
_notF = Not

_custom :: CustomFilter i -> InputFilter i
_custom = Custom

_matches :: HasFilter i => InputFilter i -> i -> Bool
_matches Always       _ = True
_matches Never        _ = False
_matches (And f1 f2)  i = _matches f1 i && _matches f2 i
_matches (Or f1 f2)   i = _matches f1 i || _matches f2 i
_matches (Not f)      i = not $ _matches f i
_matches (Custom f) i = filterMatches f i

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

_findClues
    :: forall i k r
     . Ord k
    => [k]
    -> Challenge i k r
    -> MonoidalMap [k] ClueState
_findClues _    Empty
  = mempty
_findClues kctx (Both c1 c2)
  = _findClues kctx c1 <> _findClues kctx c2
_findClues kctx (EitherC c1 c2)
  = _findClues kctx c1 <> _findClues kctx c2
_findClues _    (Gate _ _)
  = mempty
_findClues kctx (AndThen c _)
  = _findClues kctx c
_findClues kctx (RewardThen _ c)
  = _findClues kctx c
_findClues kctx (Clue k Empty)
  = singleton (kctx <> [k]) _completed
_findClues kctx (Clue k c)
  = singleton (kctx <> [k]) _seen
    <> _findClues (kctx <> [k]) c

_pumpChallenge
    :: forall i k r
     . ( Ord k
       , HasFilter i
       , Monoid r, Commutative r, Eq r
       )
    => Challenge i k r
    -> [i]
    -> (Results k r, Challenge i k r)
_pumpChallenge c
  = foldM (flip $ step []) c
  . (Nothing :)
  . fmap Just

runChallenge
    :: forall i k r.
      ( HasFilter i, Eq (CustomFilter i)
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> [i]
    -> (Results k r, Bool)
runChallenge c = fmap (== Empty) . _pumpChallenge c

_getRewards
    :: forall i k r.
      ( HasFilter i
      , Ord k
      , Monoid r, Commutative r, Eq r
      ) =>
      Challenge i k r -> [i] -> r
_getRewards c = rewards . fst . _pumpChallenge c

_getClues
    :: forall i k r.
      ( HasFilter i
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> [i]
    -> MonoidalMap [k] ClueState
_getClues c = clues . fst . _pumpChallenge c

_isEmpty
    :: forall i k r.
      ( HasFilter i, Eq (CustomFilter i)
      , Ord k
      , Monoid r, Commutative r, Eq r
      )
    => Challenge i k r
    -> Bool
_isEmpty = (== Empty) . snd . flip _pumpChallenge []

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
step _    _ Empty = pure empty

step kctx i (Both c1 c2)
  = both <$> step kctx i c1 <*> step kctx i c2

step kctx i (EitherC c1 c2) = do
  c1' <- step kctx i c1
  c2' <- step kctx i c2
  case (c1', c2') of
    (Empty, _) -> prune kctx c2'
    (_, Empty) -> prune kctx c1'
    _         -> pure $ eitherC c1' c2'

step kctx i (AndThen c1 c2) =
  step kctx i c1 >>= \case
    Empty -> step kctx Nothing c2
    c1' -> pure $ andThen c1' c2

step kctx i (RewardThen r c) = do
  _tellReward r
  step kctx i c

step kctx (Just i) (Gate f c)
  | _matches f i = step kctx Nothing c
step _    _ c@Gate{} = pure c

step kctx i (Clue k c) = do
  let kctx' = kctx <> [k]
  step kctx' i c >>= \case
    Empty -> do
      tellClue $ singleton kctx' _completed
      pure empty
    c' -> do
      tellClue $ singleton kctx' _seen
      pure $ clue [k] c'

prune
    :: (Ord k, Monoid r)
    => [k]
    -> Challenge i k r
    -> (Results k r, Challenge i k r)
prune kctx c = do
  tellClue $ fmap (<> _failed) $ _findClues kctx c
  pure empty

_tellReward
    :: (Ord k, MonadWriter (Results k r) m)
    => r -> m ()
_tellReward r = tell $ Results r mempty

tellClue
    :: (Monoid r , MonadWriter (Results k r) m)
    => MonoidalMap [k] ClueState -> m ()
tellClue k = tell $ Results mempty k

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

_bottom :: forall i k r. Challenge i k r
_bottom = gate _never empty

rewardThen
    :: forall i k r
     . (Eq r, Monoid r, Commutative r)
    => r -> Challenge i k r -> Challenge i k r
rewardThen r c | r == mempty = c
rewardThen r' (RewardThen r c) = RewardThen (r <> r') c
rewardThen r c = RewardThen r c

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

empty :: forall i k r. Challenge i k r
empty = Empty

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

_isValid
    :: forall i k r
     . Challenge i k r -> Bool
_isValid (AndThen Empty _) = False
_isValid (Both Empty _) = False
_isValid (Both _ Empty) = False
_isValid (EitherC _ Empty) = False
_isValid (EitherC Empty _) = False
_isValid (Both (RewardThen _ _) _) = False
_isValid (Both _ (RewardThen _ _)) = False
_isValid (EitherC (RewardThen _ _) _) = False
_isValid (EitherC _ (RewardThen _ _)) = False
_isValid _ = True

-- Give it a spin

type POIChallenge = Challenge Point () (Set String)

type Point = (Int, Int)

instance HasFilter Point where
  data CustomFilter Point = PointOfInterest Point
    deriving stock (Eq, Ord, Show, Generic)
  filterMatches (PointOfInterest (poiX, poiY)) (x, y) =
    x == poiX && y == poiY

main :: IO ()
main = do
  let filter' = _custom $ PointOfInterest (0, 0)
  let reward' = fromList [":D"]
  let challenge = gate filter' $ reward reward' :: POIChallenge
  let inputs = [(10, 10), (0, 0)]
  let run = runChallenge challenge inputs
  let results = fst run
  let rewards' = rewards results
  putStrLn $ show rewards'
  return ()
