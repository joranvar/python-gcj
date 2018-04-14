{-# LANGUAGE DeriveFunctor #-}
module GCJ
    ( problems
    , parseChars
    , parseGrid
    , parseNums
    , parseRepeat
    , parseWords
    , Parser
    ) where

import Control.Monad (replicateM)
import Data.List (unfoldr)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdin, stdout)

newtype Parser a = Parser
    { run :: [String] -> Maybe (a, [String])
    } deriving (Functor)
instance Applicative Parser where
    pure a = Parser $ \ss -> Just (a, ss)
    pf <*> p =
        Parser $ \ss -> do
            (a, ss') <- run p ss
            (f, ss'') <- run pf ss'
            pure (f a, ss'')
instance Monad Parser where
    ma >>= mf =
        Parser $ \ss -> do
            (a, ss') <- run ma ss
            run (mf a) ss'

problems :: Parser a -> IO [(Int, a)]
problems p = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    t <- read <$> getLine
    zip [1 .. t] . unfoldr (run p) . lines <$> getContents

parseWords :: Int -> Parser [String]
parseWords n =
    Parser $ \(s:ss) ->
        let as = take n $ words s
         in if length as /= n
                then Nothing
                else Just (as, ss)

parseChars :: Int -> Parser String
parseChars n =
    Parser $ \(s:ss) ->
        let as = take n s
         in if length as /= n
                then Nothing
                else Just (as, ss)

parseNums :: (Read a, Num a) => Int -> Parser [a]
parseNums n = map read <$> parseWords n

parseRepeat :: Int -> Parser a -> Parser [a]
parseRepeat n p = do
  as <- replicateM n p
  if length as /= n
    then Parser $ const Nothing
    else pure as

parseGrid :: (Char -> a) -> Int -> Int -> Parser [[a]]
parseGrid f r c = parseRepeat r $ map f <$> parseChars c
