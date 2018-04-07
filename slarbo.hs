#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p ghc

import Data.List (sort)
import System.IO (hSetBuffering, stdout, stdin, BufferMode(LineBuffering))

solve :: [Integer] -> String
solve ts =
  let sortedTs = sort ts
      lastEvent = minimum sortedTs
      gcdTs = gcds $ distances sortedTs
  in show $ negate (lastEvent `mod` negate gcdTs)
  where
    distances xs = zipWith (-) (tail xs) xs
    gcds = foldl1 gcd

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
    parse row = case map read $ words row of
      _:ts -> ts
      _ -> error $ "Parsing does not give ints: " ++ show row
    format r result = "Case #" ++ show r ++ ": " ++ result ++ "\n"
