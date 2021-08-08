{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module LexicalAnalyzer
 ( initialEnvironment
 , moveBack
 , nextToken
 , Token (..)
 , Environment (..)
 , ParsingException (..)
 ) where

import Control.Applicative (liftA2)
import Control.Exception (Exception, throwIO)
import Control.Lens (makeLenses, view, (%~), (.~), (^.))
import Control.Monad.Except (liftIO)
import Control.Monad.State (StateT, gets, modify)
import Data.Char (isDigit, isSpace, ord)
import qualified Data.Map as Map (Map, fromList, lookup, member, (!))
import Data.Typeable (Typeable)
import qualified System.IO.Streams as Streams (InputStream, read)

newtype ParsingException
  = ParsingException String
  deriving (Show, Typeable, Exception)

data Token
  = BEGIN
  | DEF
  | NAME String
  | LPAREN
  | RPAREN
  | COMMA
  | EQUALS
  | EPS
  | END
  deriving Eq

instance Show Token where
  show BEGIN       = "s"
  show DEF         = "def"
  show (NAME name) = name
  show LPAREN      = "("
  show RPAREN      = ")"
  show COMMA       = ","
  show EQUALS      = "="
  show EPS         = "ε"
  show END         = "$"


data Environment
  = Environment
  { _stream   :: Streams.InputStream Char
  , _char     :: Char
  , _position :: Int
  , _token    :: Token
  , _skip     :: Bool
  }

makeLenses ''Environment

instance Show Environment where
  show env = "Current char = " <> show (env ^. char) <> ", " <>
             "Current position = " <> show (env ^. position) <> ", " <>
             "Current token = " <> show (env ^. token)

initialEnvironment :: Streams.InputStream Char -> Environment
initialEnvironment inputStream = Environment inputStream '\0' (-1) BEGIN True

tokenByKeyword :: Map.Map String Token
tokenByKeyword = Map.fromList [("def", DEF)]

tokenBySymbol :: Map.Map Char Token
tokenBySymbol = Map.fromList [(',', COMMA), ('=', EQUALS), ('(', LPAREN), (')', RPAREN), ('$', END)]

isLatinLetter :: Char -> Bool
isLatinLetter c = ((ord c >= ord 'a') && (ord c <= ord 'z')) || ((ord c >= ord 'A') && (ord c <= ord 'Z'))

startsWith :: Char -> Bool
startsWith = liftA2 (||) isLatinLetter (== '_')

endsWith :: Char -> Bool
endsWith = liftA2 (||) startsWith isDigit

moveBack :: StateT Environment IO ()
moveBack = modify $ skip.~ False

nextChar :: StateT Environment IO Char
nextChar = do
  skipFlag <- gets $ view skip
  if skipFlag
  then do
    is <- gets $ view stream
    maybeChar <- liftIO $ Streams.read is
    curChar <- case maybeChar of
      Nothing -> return '$'
      Just c  -> return c
    modify $ position %~ (+1)
    modify $ char .~ curChar
    return curChar
  else do
    modify $ skip .~ True
    gets $ view char

readWord :: StateT Environment IO String
readWord = do
  c <- nextChar
  if endsWith c
    then fmap (c :) readWord
    else do return []

nextToken :: StateT Environment IO Token
nextToken = do
  curPosition <- gets $ view position
  curChar <- nextChar
  curToken <- case curChar of
    space
      | isSpace space -> nextToken
    letter
      | letter `Map.member` tokenBySymbol -> return $ tokenBySymbol Map.! letter
      | startsWith letter -> do
        wordContinuation <- readWord
        let word = letter : wordContinuation
        let maybeToken = Map.lookup word tokenByKeyword
        moveBack
        return $ case maybeToken of
          Nothing -> NAME word
          Just t  -> t
    symbol -> liftIO $ throwIO $ ParsingException $
      "Illegal character " ++ show symbol ++ " in position " ++ show (curPosition + 1)
  modify $ token .~ curToken
  return curToken
