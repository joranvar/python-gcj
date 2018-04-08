import Data.List (sort, partition)
import System.IO (hSetBuffering, stdout, stdin, BufferMode(LineBuffering))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  interact mymain

mymain :: String -> String
mymain input =
  let _:rows = lines input
      vRows = map snd . filter fst . zip (cycle [False, True]) $ rows
  in concat $ zipWith format [1..] $ map (solve . parse) vRows
  where
    parse = map read . words
    format r result = "Case #" ++ show r ++ ": " ++ result ++ "\n"

troubleSort :: (Ord a) => [a] -> [a]
troubleSort =
  merge . (\(odds, evens) -> (sort . map snd $ odds, sort . map snd $ evens)) . partition fst . zip (cycle [True, False])
  where merge :: ([a], [a]) -> [a]
        merge ([], []) = []
        merge ([x], []) = [x]
        merge (a:as, b:bs) = a:b:merge (as, bs)

solve :: [Integer] -> String
solve = unsortedIndex 0 . troubleSort
  where unsortedIndex :: (Ord a) => Integer -> [a] -> String
        unsortedIndex _ [_] = "OK"
        unsortedIndex n (x:y:rest) | x > y = show n
        unsortedIndex n (_:rest) = unsortedIndex (n+1) rest
