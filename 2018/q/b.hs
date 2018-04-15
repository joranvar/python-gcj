import Data.Bifunctor (second)
import Data.List (sort, partition)
import GCJ

main :: IO ()
main = putStr =<< unlines . map (format . second solve) <$> problems parser
  where format (r, result) = "Case #" ++ show r ++ ": " ++ result

parser :: Parser [Integer]
parser = do
  [n] <- parseNums 1
  parseNums n

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
