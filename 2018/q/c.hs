import Data.List (delete)
import System.IO (hSetBuffering, stdout, stdin, BufferMode(LineBuffering))
--DEBUG: import System.IO (stderr, hPutStr)

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
--DEBUG:           deriving Show

isSolved :: State -> Bool
isSolved (EndState b) = b
isSolved _ = False

isEndState :: State -> Bool
isEndState (EndState _) = True
isEndState _ = False

interactiveCase :: State -> IO State
interactiveCase caseNo = flip (iterateUntilM isEndState) caseNo $ \state -> do
  line <- getLine
--DEBUG:   hPutStr stderr $ "-> " ++ line ++ "\n"
  let (output, state') = mymain state line
--DEBUG:   hPutStr stderr $ "<- " ++ output
--DEBUG:   hPutStr stderr $ show state' ++ "\n"
  putStr output
  pure state'

newtype P = P { want :: [(Int, Int)] } -- deriving Show

mymain :: State -> String -> (String, State)
mymain _ "-1 -1" = ("", EndState False)
mymain _ "0 0" = ("", EndState True)
mymain (InitialState t) a = let (output, state) = solve $ initialState (read a) in (output, InProgressState t state)
mymain (InProgressState t p) xy = let [x,y] = map read $ words xy; (output, p') = solve $ addState p x y in (output, InProgressState t p')

solve :: P -> (String, P)
solve p =
  let want' = want p
      (x, y) = head want'
      minx = (1 +) $ minimum $ map fst want'
      miny = (1 +) $ minimum $ map snd want'
      maxx = (-1 +) $ maximum $ map fst want'
      maxy = (-1 +) $ maximum $ map snd want'
  in (show (clamp minx maxx x) ++ " " ++ show (clamp miny maxy y) ++ "\n", p)
  where clamp lower upper = max 2 . min upper . max lower

initialState :: Int -> P
initialState a =
  let (x', y') = case a of 20 -> (4, 5); 200 -> (10, 20)
  in P [(x,y) | x <- [1..x'], y <- [1..y']]

addState :: P -> Int -> Int -> P
addState p x y = P { want = delete (x,y) $ want p }
