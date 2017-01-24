---------------------------------------------------------------------------------
-- 2. Calculate probability of winning using simulation {reposts}
--
-- There's a contest going on in a Russian social network: seven prizes will be
-- given to seven randomly chosen people among those who have reposted a certain
-- post. (There are actually 100 prizes, but the other 93 suck, so we'll ignore
-- them.) There are already ~1000000 reposts. My sister wonders: what's the
-- probability of her winning at least one prize (out of those seven) if she
-- reposts the post 10 times (from different accounts)? What about 100 times?
-- 1000 times?
--
-- Calculate the answer by running a simulation some number of times (for
-- instance, 10000 times). You can use System.Random or some other random library
-- (e.g. Data.Random).
--
-- If you're not good at probabilistic simulations, here's a hint:
-- https://github.com/neongreen/haskell-ex/blob/master/week1/HINTS.md#reposts
---------------------------------------------------------------------------------
module Main where

main :: IO ()
main = do
  simlulation probSisterWinsWith 10
  simlulation probSisterWinsWith 1000

  where
    probSisterWinsWith = probability 7 1 1000000


type TotalPrizes = Integer
type MinWins = Integer
type TotalReposts = Integer
type NumReposts = Integer
type Probability = Double

probability :: TotalPrizes -> MinWins -> TotalReposts -> NumReposts -> Double
probability =

simlulation :: (NumReposts -> Double) -> IO Double

