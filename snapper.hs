#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p ghc

inputFileName = "/home/joranvar/A-large-practice.in"
outputFileName = "output_large.txt"

solve :: (Int, Int) -> String
solve (n, k)
    | (k+1) `mod` (2 ^ n) == 0 = "ON"
    | otherwise = "OFF"

main :: IO ()
main = do
  _:rows <- lines <$> readFile inputFileName
  writeFile outputFileName $ concat $ zipWith format [1..] $ map (solve . parse) rows
  where
    parse row = case map read $ words row of
      [n, k] -> (n, k)
      _ -> error $ "Parsing does not give two ints: " ++ show row
    format r result = "Case #" ++ show r ++ ": " ++ result ++ "\n"
