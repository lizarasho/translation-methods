module Utils where

import Control.Monad.State (evalStateT)
import LexicalAnalyzer (initialEnvironment)
import Parser (SyntaxTree(..), parse)
import qualified System.IO.Streams as Streams (InputStream, fromList, makeInputStream)

stdin :: IO (Streams.InputStream Char)
stdin = Streams.makeInputStream $ fmap Just getChar

size :: SyntaxTree -> Int
size (Leaf _)            = 1
size (Branch _ children) = 1 + sum (map size children)

helper :: (Show a, Show b, Show c) => a -> b -> c -> String -> [String]
helper label parentCounter ownCounter color =
  [ show parentCounter <> " -> " <> show ownCounter
  , show ownCounter <> " [label=\"" <> show label <> "\"" <> color <> "]" ]

toList :: Int -> Int -> SyntaxTree -> [String]
toList parentCounter ownCounter (Leaf label) = helper label parentCounter ownCounter ", color=\"red\""
toList parentCounter ownCounter (Branch label children) = let
  counters = map (\ind -> 1 + ownCounter + sum (map size $ take ind children)) [0 .. length children - 1]
  in helper label parentCounter ownCounter []
    <> concatMap (\(tree, counter) -> toList ownCounter counter tree) (zip children counters)

-- | 1.png <-> 'def functionName(a,b,c)='
-- | 2.png <-> 'def f()='
-- | 3.png <-> 'def g(x)='
generateGraph :: IO ()
generateGraph = do
  inputStream <- Streams.fromList "def g(x)="
  let env = initialEnvironment inputStream
  tree <- evalStateT parse env
  let output = unlines $ tail $ toList (-1) 0 tree
  putStrLn output
