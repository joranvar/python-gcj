{-# LANGUAGE DeriveFunctor #-}
module GCJ
    ( problems
    , parseNums
    , parseRepeat
    , Parser
    ) where

import Data.List (unfoldr)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdin, stdout)

newtype Parser a = Parser
    { run :: [String] -> Maybe (a, [String])
    } deriving (Functor)
instance Applicative Parser where
    pure a = Parser $ \ss -> Just (a, ss)
    pf <*> p =
        Parser $ \ss ->
            case run p ss of
                Just (a, ss') ->
                    case run pf ss' of
                        Just (f, ss'') -> Just (f a, ss'')
                        Nothing -> Nothing
                Nothing -> Nothing
instance Monad Parser where
    ma >>= mf =
        Parser $ \ss ->
            case run ma ss of
                Just (a, ss') -> run (mf a) ss'
                Nothing -> Nothing

problems :: Parser a -> IO [(Int, a)]
problems p = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    t <- read <$> getLine
    zip [1 .. t] . unfoldr (run p) . lines <$> getContents

parseNums :: (Read a, Num a) => Int -> Parser [a]
parseNums n =
    Parser $ \(s:ss) ->
        let as = take n $ map read $ words s
         in if length as /= n
                then Nothing
                else Just (as, ss)

parseRepeat :: Int -> Parser [a] -> Parser [[a]]
parseRepeat n p =
    Parser $ \ss ->
        let as =
                take n $
                iterate
                    (\(Just (as', ss')) ->
                         case run p ss' of
                             Just (a, rest) -> Just (as' ++ [a], rest)
                             Nothing -> Nothing)
                    (Just ([], ss))
         in if length as /= n
                then Nothing
                else last as
