{-# LANGUAGE LambdaCase #-}

import Data.Functor (($>))
import Data.List (sort, group, genericLength)
import System.IO (hSetBuffering, stdout, stdin, BufferMode(LineBuffering))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  interact mymain

mymain :: String -> String
mymain input =
  let _:rows = lines input
  in concat $ zipWith format [1..] $ map (solve . parse) rows
  where
    parse row = case words row of
      [d', p'] -> (read d', p')
    format r result = "Case #" ++ show r ++ ": " ++ result ++ "\n"

minValue :: [(Integer, Integer)] -> Integer
minValue = sum . map snd

value :: [(Integer, Integer)] -> Integer
value = sum . map (uncurry (*))

toSlots :: String -> [(Integer, Integer)]
toSlots = dropWhile ((==0) . snd) . reverse . zip (map (2^) [0..]) . concatMap (\case ('C':cs) -> cs $> 0; ss@_ -> [genericLength ss]) . group . ('C':)

decrease :: [(Integer, Integer)] -> Integer -> Integer
decrease ((_, count):(value', count'):rest) delta =
  let need = abs $ delta `div` (-value')
      taken = min need count
      delta' = delta - (taken * value')
      newList = (value', count' + taken):rest
  in if delta' > 0 then taken + decrease newList delta' else taken
decrease a b = error $ show (a,b)

solve :: (Integer, String) -> String
solve (d, p) | minValue (toSlots p) > d = "IMPOSSIBLE"
solve (d, p) | value (toSlots p) <= d = "0"
solve (d, p) =
  let slots = toSlots p
      delta = value slots - d
  in show $ decrease slots delta
