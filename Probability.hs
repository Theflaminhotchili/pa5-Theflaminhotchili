import Data.List (sortBy, groupBy)
import Data.Ord  (comparing)
import Data.Function (on)

newtype Probability a
  = Probability { getProbabilities :: [(a,Double)] }
    deriving Show

roll :: Probability Int
roll = Probability [ (i,1/6) | i <- [1..6] ]



instance Functor Probability where
  fmap f (Probability xs) = Probability [(f a, p) | (a, p) <- xs]

instance Applicative Probability where
  pure a = Probability [(a,1.0)]
  (Probability fs) <*> (Probability xs) =
    Probability [(f a, pf*px) | (f,pf) <- fs, (a,px) <- xs]

instance Monad Probability where
  (Probability xs) >>= k =
    Probability [(b, p*q) | (a,p) <- xs, (b,q) <- getProbabilities (k a)]


twoRolls :: Probability Int
twoRolls = do
  a <- roll
  b <- roll
  pure $ a + b

normalize :: (Ord a) => Probability a -> Probability a
normalize (Probability xs) =
  let sortedxs = sortBy (comparing fst) xs
      groupedxs = groupBy ((==) `on` fst) sortedxs
      sumGroup = map sumGroups groupedxs
  in Probability sumGroup
  where
    sumGroups group = (fst (head group),sum (map snd group))

rollDice :: Int -> Probability Int
rollDice n = foldl step (pure 0) [1..n]
  where
  step acc _ =
    normalize $ do
    a <- acc
    r <- roll
    pure (a + r)