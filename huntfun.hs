#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.generic-data p.monoid-subclasses p.monoidal-containers p.multiset])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module HuntFun where

import           Control.Applicative (liftA2)
import           Control.Monad.ST
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Foldable
import           Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as M
import           Data.Monoid
import           Data.Monoid.Cancellative
import           Data.STRef
import           Data.Semigroup
import           Data.Set (Set, fromList)
import qualified Data.Set as S
import           GHC.Generics
import           Generic.Data

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

always :: InputFilter i
always = Always

never  :: InputFilter i
never = Never

andF :: InputFilter i -> InputFilter i -> InputFilter i
andF = And

orF  :: InputFilter i -> InputFilter i -> InputFilter i
orF = Or

notF :: InputFilter i -> InputFilter i
notF = Not

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

newtype Challenge i k r = Challenge
  { unChallenge
        :: forall s
         . DList k
        -> (DList k -> ClueState
                    -> ST s ClueState)
        -> ST s (ChallengeData i k r s)
        -> ST s (ChallengeData i k r s)
  }

instance ( Show (CustomFilter i), Ord (CustomFilter i)
         , Ord k, Show k
         , Monoid r, Show r
         )
      => Show (Challenge i k r) where
  show (Challenge g) =
    runST $ fmap show $ g mempty (const $ pure . id) end

instance (Semigroup r, Ord k, Ord (CustomFilter i))
      => Semigroup (Challenge i k r) where
  Challenge c1 <> Challenge c2 =
    Challenge $ \kctx rec cont -> do
        d1 <- c1 kctx rec cont
        d2 <- c2 kctx rec cont
        pure $ d1 <> d2

instance (Monoid r, Ord k, Ord (CustomFilter i))
      => Monoid (Challenge i k r) where
  mempty = Challenge $ \_ -> pure mempty

data ChallengeData i k r s = ChallengeData
  { waitingOn
      :: !(MonoidalMap
            (InputFilter i)
            (ST s (ChallengeData i k r s)))
  , results    :: !(Results k r)
  , isComplete :: !Any
  }
  deriving stock (Generic)

deriving via Generically (ChallengeData i k r s)
  instance (Ord k, Semigroup r, Ord (CustomFilter i))
    => Semigroup (ChallengeData i k r s)

deriving via Generically (ChallengeData i k r s)
  instance (Ord k, Monoid r, Ord (CustomFilter i))
    => Monoid (ChallengeData i k r s)

instance (Show k, Show (CustomFilter i), Show r)
      => Show (ChallengeData i k r s) where
  show (ChallengeData ri r (Any res)) = mconcat
    [ "Challenge { waitingFor = "
    , show $ M.keys ri
    , ", result = "
    , show res
    , ", rewards = "
    , show r
    , " }"
    ]

tellClue
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => MonoidalMap [k] ClueState
    -> ChallengeData i k r s
tellClue ks =
  mempty { results = Results mempty ks }

tellReward
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => r
    -> ChallengeData i k r s
tellReward r = mempty { results = Results r mempty }

rewardThen
    :: (Ord (CustomFilter i), Ord k, Monoid r, Ord k)
    => r
    -> Challenge i k r
    -> Challenge i k r
rewardThen r (Challenge c) =
  Challenge $ \kctx rec cont -> do
    d <- c kctx rec cont
    pure $ tellReward r <> d

decorate
    :: Ord k
    => STRef s Bool
    -> STRef s (Set (DList k))
    -> (DList k -> ClueState -> ST s ClueState)
    -> DList k
    -> ClueState
    -> ST s ClueState
decorate filled ref rec k cs = do
  readSTRef filled >>= \case
    True -> rec k failed
    False -> do
      modifySTRef' ref $ S.insert k
      rec k cs

prune
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => STRef s (Set (DList k))
    -> ST s (ChallengeData i k r s)
prune ref = do
  ks <- readSTRef ref
  pure $ flip foldMap ks $ \k ->
    tellClue $ M.singleton (DL.toList k) failed

oneshot :: Monoid a => STRef s Bool -> ST s a -> ST s a
oneshot ref m =
  readSTRef ref >>= \case
    True  -> pure mempty
    False -> do
      writeSTRef ref True
      m

end
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => ST s (ChallengeData i k r s)
end = pure $ mempty { isComplete = Any True }

runChallenge
    :: forall i k r
     . ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => [i] -> Challenge i k r -> (Results k r, Bool)
runChallenge evs (Challenge c) = runST $ do
  d' <-
    pumpChallenge evs =<<
      c mempty
        (const $ pure . id)
        end
  pure (results d', getAny $ isComplete d')

pumpChallenge
    :: ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => [i]
    -> ChallengeData i k r s
    -> ST s (ChallengeData i k r s)
pumpChallenge [] d = pure d
pumpChallenge _ d
  | getAny $ isComplete d
  = pure d
pumpChallenge (ri : es) d =
  pumpChallenge es =<< step ri d

step
    :: forall i k r s.
       ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => i
    -> ChallengeData i k r s
    -> ST s (ChallengeData i k r s)
step ri d = do
  let efs = M.assocs $ waitingOn d
  (endo, ds) <-
    flip foldMapM efs $ \(ef, res) ->
      case matches ef ri of
        True -> do
          d' <- res
          pure (Endo $ M.delete ef, d')
        False -> mempty
  pure $
    d { waitingOn =
           appEndo endo $ waitingOn d
       } <> ds

foldMapM
    :: (Monoid m, Applicative f, Traversable t)
    => (a -> f m)
    -> t a
    -> f m
foldMapM f = fmap fold . traverse f

-- Public interface

empty :: Challenge i k r
empty = Challenge $ \_ _ cont -> cont

reward
    :: ( Ord k, Ord (CustomFilter i)
       , Commutative r, Monoid r
       )
    => r
    -> Challenge i k r
reward r = rewardThen r empty

clue
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r)
    => [k]
    -> Challenge i k r
    -> Challenge i k r
clue [] c = c
clue (k : ks) c =
  Challenge $ \kctx rec cont -> do
    let kctx' = kctx <> DL.singleton k
        k' = DL.toList kctx'
    state <- rec kctx' seen
    d <- unChallenge (clue ks c) kctx' rec $ do
      dc <- cont
      pure $ tellClue (M.singleton k' completed) <> dc
    pure $ tellClue (M.singleton k' state) <> d

eitherC
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
eitherC (Challenge c1) (Challenge c2) =
  Challenge $ \kctx rec cont -> do
    filled  <- newSTRef False
    c1_clues <- newSTRef mempty
    c2_clues <- newSTRef mempty
    d1 <-
      c1 kctx (decorate filled c1_clues rec) $
        oneshot filled $ do
          d <- cont
          p <- prune c2_clues
          pure $ d <> p
    d2 <-
      c2 kctx (decorate filled c2_clues rec) $
        oneshot filled $ do
          d <- cont
          p <- prune c1_clues
          pure $ d <> p
    pure $ d1 <> d2

andThen
    :: Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
andThen (Challenge c1) (Challenge c2) =
  Challenge $ \kctx rec cont ->
    c1 kctx rec (c2 kctx rec cont)

both
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => Challenge i k r
    -> Challenge i k r
    -> Challenge i k r
both (Challenge c1) (Challenge c2) =
  Challenge $ \kctx rec cont -> do
    remaining_wins  <- newSTRef @Int 2
    let run_win = do
          modifySTRef' remaining_wins $ subtract 1
          readSTRef remaining_wins >>= \case
            0 -> cont
            _ -> pure mempty
    liftA2 (<>)
      (c1 kctx rec run_win)
      (c2 kctx rec run_win)

gate
    :: forall i k r
     . (Ord (CustomFilter i), Ord k, Monoid r)
    => InputFilter i
    -> Challenge i k r
    -> Challenge i k r
gate ef (Challenge c) = Challenge $ \kctx rec cont ->
  pure $ (mempty @(ChallengeData i k r _))
    { waitingOn = M.singleton ef $ c kctx rec cont }

bottom
    :: (Ord (CustomFilter i), Ord k, Monoid r)
    => Challenge i k r
bottom = Challenge $ \_ -> mempty

getClues
    :: forall i k r.
       ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => Challenge i k r
    -> [i]
    -> MonoidalMap [k] ClueState
getClues c = clues . fst . flip runChallenge c

getRewards
    :: forall i k r.
       ( HasFilter i, Ord (CustomFilter i)
       , Ord k
       , Monoid r
       )
    => Challenge i k r
    -> [i]
    -> r
getRewards c = rewards . fst . flip runChallenge c

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
