#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.split])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DerivingVia #-}

import Control.Applicative
import Data.Functor.Compose
import Data.List.Split
import GHC.Float
import Data.Monoid

-- Tile public interface + implementation

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

-- Give it a spin

diagonalTile :: Tile (Sum Int)
diagonalTile =
    Tile { sample = \x y -> if x == y then Sum 1 else Sum 0 }

toString :: Int -> Tile (Sum Int) -> String
toString squareLength' tile =
    let squareLength = if mod squareLength' 2 == 0 then squareLength' + 1 else squareLength'
        quadrantLength = int2Double (squareLength - 1)/2
        coords = do
          r <- (/quadrantLength) <$> [-quadrantLength..quadrantLength]
          c <- (/quadrantLength) <$> [-quadrantLength..quadrantLength]
          return (c, r)
        getSample (Tile s) x y = s x y
        render (Sum i) = if i == 1 then "██" else "░░"
        rendered = render <$> (uncurry (getSample tile) <$> coords)
        stringified = unlines $ concat <$> chunksOf squareLength rendered
    in stringified

main :: IO ()
main = do
    let squareLength = 20
    putStrLn (toString squareLength diagonalTile)
    putStrLn (toString squareLength (swirl diagonalTile))
    -- The strict equality in diagonalTile make these renderings a bit not very pretty
    putStrLn (toString squareLength (beside (flipV diagonalTile) (flipH diagonalTile)))
    putStrLn (toString squareLength (behind (above diagonalTile Main.empty) (above Main.empty diagonalTile)))
    return ()
