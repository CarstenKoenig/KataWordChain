{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (guard)

import Data.Char (toLower)

import Data.List (sort, foldl')
import Data.Maybe (fromMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)


type Dictionary = HashMap String (HashSet String)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  
  putStr "creating dictionary..."
  !wörterbuch <- createDictionary <$> readWords
  putStrLn "done"
  repl wörterbuch


repl :: Dictionary -> IO ()
repl dict = do
  putStr "Start-Word: "
  start <- getLine
  putStr "Goal-Word: "
  if null start then do
    putStrLn "bye..."
  else do
    goal <- getLine
    case findPath dict (start, goal) of
      Nothing -> putStrLn "found no path"
      Just path -> print path
    repl dict


findPath :: Dictionary -> (String, String) -> Maybe [String]
findPath dict (start, goal) =
  breadthFirstSearch dict goal S.empty [[start]]


breadthFirstSearch :: Dictionary -> String -> HashSet String -> [[String]] -> Maybe [String]
breadthFirstSearch _ _ _ [] = Nothing
breadthFirstSearch dict word visited (path @ (start:_) : nexts)
  | start == word = Just $ reverse path
  | start `S.member` visited = breadthFirstSearch dict word visited nexts
  | otherwise =
      let visited' = S.insert start visited
          nexts' = map (:path) $ childNodes dict start
      in breadthFirstSearch dict word visited' (nexts ++ nexts')


childNodes :: Dictionary -> String -> [String]
childNodes dict word = do
  k <- keys word
  case M.lookup k dict of
    Nothing -> pure []
    Just set -> filter (/= word) (S.toList set)


createDictionary :: [String] -> Dictionary
createDictionary =
  M.filter ((> 1) . S.size) . foldl' insertWord M.empty


insertWord :: Dictionary -> String -> Dictionary
insertWord dict word =
  foldl' (\m k -> M.alter insert k m) dict $ keys word
  where insert = Just . S.insert word . fromMaybe S.empty


readWords :: IO [String]
readWords =
  map normalize .
  filter ((>= 3) . length) .
  lines <$>
  readFile "c:/temp/wordlist.txt"


normalize :: String -> String
normalize = map toLower . takeWhile (/= '\'')


keys :: String -> [String]
keys [] = []
keys [c] = ["_"]
keys (c:cs) = ('_':cs) : map (c:) (keys cs)
