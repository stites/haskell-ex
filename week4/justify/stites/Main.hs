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

main :: IO ()
main = do
  putStr "enter column width: "
  strWidth <- getLine
  let width = read strWidth :: Int
  putStr "enter text to justify: "
  strWidth <- getLine

  putStrLn "output: "
  return ()

lengths :: [String] -> [Int]
lengths = fmap length

txtWidth :: [Int] -> [Int]
txtWidth = tail . scanl (+) 0

textToLines :: String -> [(String, Int)]
textToLines str = let
    ws = words str
 in
   zip ws (lengths $ ws)

scansToLine :: Int -> [(String, Int)] -> [([String], Int)]
scansToLine txtwidth = reverse . go [([""], -1)]
  where
    go :: [([String], Int)] -> [(String, Int)] -> [([String], Int)]
    go acc     [] = acc
    go acc ((w, wlen):more) =
      let
        (ws, llen) = head acc
        nxtLen = wlen + llen
      in
        if nxtLen <= txtwidth
           then go ((w:ws, also nxtLen txtwidth):(tail acc)) more
           else go (([w], wlen):acc) more

    also nxtLen textwidth = if nxtLen == txtwidth then nxtLen else nxtLen+1



