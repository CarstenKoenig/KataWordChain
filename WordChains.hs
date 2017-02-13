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


type Wörterbuch = HashMap String (HashSet String)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  
  putStr "erstelle Wörterbuch..."
  !wörterbuch <- erstelleWörterbuch <$> wörter
  putStrLn "fertig"
  repl wörterbuch


repl :: Wörterbuch -> IO ()
repl wörterbuch = do
  putStr "Von: "
  von <- getLine
  putStr "Nach: "
  if null von then do
    putStrLn "bye..."
  else do
    nach <- getLine
    case findePfad wörterbuch (von, nach) of
      Nothing -> putStrLn "Kein Weg gefunden"
      Just pfad -> print pfad
    repl wörterbuch


findePfad :: Wörterbuch -> (String, String) -> Maybe [String]
findePfad wörterbuch (von, nach) = suche wörterbuch nach S.empty [[von]]


suche :: Wörterbuch -> String -> HashSet String -> [[String]] -> Maybe [String]
suche _ _ _ [] = Nothing
suche wörterbuch nach visited (pfad @ (von:_) : nexts)
  | von == nach = Just $ reverse pfad
  | von `S.member` visited = suche wörterbuch nach visited nexts
  | otherwise =
      let visited' = S.insert von visited
          nexts' = map (:pfad) $ kinder wörterbuch von
      in suche wörterbuch nach visited' (nexts ++ nexts')


kinder :: Wörterbuch -> String -> [String]
kinder wörterbuch wort = do
  k <- keys wort
  case M.lookup k wörterbuch of
    Nothing -> pure []
    Just set -> filter (/= wort) (S.toList set)


erstelleWörterbuch :: [String] -> Wörterbuch
erstelleWörterbuch =
  M.filter ((> 1) . S.size) . foldl' einfügen M.empty


einfügen :: Wörterbuch -> String -> Wörterbuch
einfügen wörterbuch wort =
  foldl' (\m k -> M.alter insert k m) wörterbuch $ keys wort
  where insert = Just . S.insert wort . fromMaybe S.empty


wörter :: IO [String]
wörter =
  map kanonisch .
  filter ((>= 3) . length) .
  lines <$>
  readFile "c:/temp/wordlist.txt"


kanonisch :: String -> String
kanonisch = map toLower


keys :: String -> [String]
keys [] = []
keys [c] = ["_"]
keys (c:cs) = ('_':cs) : map (c:) (keys cs)
