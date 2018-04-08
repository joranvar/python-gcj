import Data.List (delete)
import System.IO (hSetBuffering, stdout, stdin, BufferMode(LineBuffering))

-- From Control.Monad.Loops:
infixr 0 `iterateUntilM`
-- | Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
  | p v       = return v
  | otherwise = f v >>= iterateUntilM p f

{-# SPECIALIZE allM :: (a -> IO Bool) -> [a] -> IO Bool #-}
-- | short-circuit 'all' with a \"monadic predicate\".
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x:xs)   = do
  q <- p x
  if q
    then allM p xs
    else return False

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  t <- read <$> getLine
  _ <- allM (fmap isSolved . interactiveCase . InitialState) [1..t]
  pure ()

data State = InitialState Int
           | InProgressState Int P
           | EndState Bool

isSolved :: State -> Bool
isSolved (EndState b) = b
isSolved _ = False

isEndState :: State -> Bool
isEndState (EndState _) = True
isEndState _ = False

interactiveCase :: State -> IO State
interactiveCase caseNo = flip (iterateUntilM isEndState) caseNo $ \state -> do
  line <- getLine
  let (output, state') = mymain state line
  putStr output
  pure state'

newtype P = P ()

mymain :: State -> String -> (String, State)
mymain (InitialState t) _ = ("", InProgressState t (P ()))
mymain (InProgressState t _) row = (format . solve . parse $ row, EndState True)
  where
    parse = map read . words
    format result = "Case #" ++ show t ++ ": " ++ result ++ "\n"

solve :: [Integer] -> String
solve = show . sum
