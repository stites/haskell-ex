--------------------------------------------------------------------------------
-- | === Justify text `{justify}`
--
-- Given a string, format it to fit N character lines and justify text inside.
--
-- Input:
--
-- @
-- > 65
-- > It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness...
-- @
--
-- Output:
--
-- @
-- It was the best of  times, it was the worst  of times, it was the
-- age of wisdom, it was the age of foolishness, it was the epoch of
-- belief, it was  the epoch of  incredulity, it  was the  season of
-- Light, it was the season of Darkness...
-- @
--
-- To justify text, you can simply make some spaces double (or triple if needed,
-- etc). For bonus points, try to choose positions of spaces nicely (e.g. in
-- the first line there are 14 spaces, and 2 of them have to be double if we
-- want to make the line be 65 characters long; we could choose first and
-- second space for that, but instead we chose 5th and 10th so that the
-- lengths of word groups would be approximately equal).
--
-- Don't justify the last line or it'll look really ugly.
--------------------------------------------------------------------------------
module Main where

-- import Data.Sequence (Seq)
-- import qualified Data.Sequence as S
import Debug.Trace
import Data.List

main :: IO ()
main = do
  putStr "enter column width: "
  strWidth <- getLine
  let width = read strWidth :: Int
  putStr "enter text to justify: "
  text <- getLine
  let q = wordsWithLength $ text
  -- putStrLn $ show q
  let ws = fmap (\(ss, i) -> (onInit (++" ") ss, i)) $ reverse $ wordsToLines width q
  -- putStrLn $ show ws
  let ws' = map (\(ln, l) -> (ln, width - l)) ws
  let ls = map (\(ln, s) -> fillSpaces s ln) ws'
  let ls' = map (\(rem, ln) -> snd $ injectSpaces_ rem ln) ls

  putStrLn "output: "
  printJusified ls' (map fst ws)

  return ()
  where
    putMergedLine :: [String] -> IO ()
    putMergedLine ss = do
      putStr $ intercalate "" ss
      putStrLn $ "--" ++ (show . length) ss

    printJusified :: [[String]] -> [[String]] -> IO ()
    printJusified justified original = do
      mapM_ putMergedLine . init $ justified
      putMergedLine . last $ original

wordsWithLength :: String -> [(String, Int)]
wordsWithLength str = let ws = words str in zip ws (fmap length $ ws)

onInit :: (a -> a) -> [a] -> [a]
f `onInit` [] = []
f `onInit` ss = (fmap f $ init ss) ++ [last ss]

wordsToLines :: Int -> [(String, Int)] -> [([String], Int)]
wordsToLines txtwidth = go [([""], -1)]
  where
    go :: [([String], Int)] -> [(String, Int)] -> [([String], Int)]
    go acc               [] = acc
    go acc ((w, wlen):more) =
      let
        (ws, llen) = head acc
        lineLen = wlen + llen
        continue =
          if lineLen <= txtwidth
          then (ws ++ [w], lenWithSpace lineLen):(tail acc)
          else ([w++" "], wlen):acc
      in
        go continue more

    lenWithSpace ll = if ll == txtwidth then ll else ll+1
    wrdWithSpace ll w = if ll == txtwidth then w else w ++ " "

spacesToAdd :: Int -> [([String], Int)] -> [Int]
spacesToAdd width = fmap ((-) width . snd)

fillSpaces :: Int -> [String] -> (Int, [String])
fillSpaces nspaces strs = let
    l = length strs
    d = nspaces `div` l
    r = nspaces `mod` l
    filler = replicate (nspaces `div` (length strs)) ' '
 in (r, fmap (:[]) . intercalate filler $ strs)

injectSpaces_ :: Int -> [String] -> (Int, [String])
injectSpaces_ nspaces [] = (nspaces, [])
injectSpaces_ nspaces ss = let
    (nextspaces, ns) = foldr go (nspaces, []) (init ss)
  in (nextspaces,  ns ++ [last ss])
  where
    go :: String -> (Int, [String]) -> (Int, [String])
    go next (nspaces, memo) = (nspaces - 1, (ammendSpace nspaces next) ++ memo)

    ammendSpace :: Int -> String -> [String]
    ammendSpace n s = if n <= 0 then [s] else [s, " "]




