module Main where

import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad (when)

trigrams = S.fromList [ "ate","dis","for","hue","awl","mis","dim","con","age"]

{-
  for each word:
  - find up to three way we can allocate two trigrams
  - for each of these ways try remaining words and trigrams
-}

find _ _ _ [] = do
  return ()

find allwords s found _ | S.size s == 3 = do
  let remainders = map concat $ L.permutations $ map snd found
  let ws = filter (\w -> S.member w allwords) remainders
  case ws of
    [] -> return ()
    v -> do print ("FOUND, rest:", s)
            mapM_ print found
            print v


find allw tri found (w:ws) = do
  let fst = take 3 w
      snd = take 3 $ drop 3 w
      trd = take 3 $ drop 6 w

  attempt fst snd trd
  attempt snd trd fst
  attempt trd fst snd

  find allw tri found ws

  where
    attempt fst snd remainder =
      when (valid fst && valid snd) $ do
         let rest = (without fst snd tri)
         --print ("Found",fst,snd,w, rest)
         find allw rest ((w,remainder):found) ws

    valid t = S.member t tri

without :: String -> String -> S.Set String -> S.Set String
without a b s = S.delete b (S.delete a s)


main = do
  words <- readFile "9letters.txt"
  let allwords = S.fromList $ lines words
  find allwords trigrams [] $ lines words
