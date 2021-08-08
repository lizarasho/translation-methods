{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser
  ( SyntaxException(..)
  , SyntaxTree(..)
  , NonTerminal(..)
  , parse) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.State (MonadIO, StateT, liftIO)
import Data.Typeable (Typeable)
import LexicalAnalyzer

newtype SyntaxException
  = SyntaxException String
  deriving (Show, Typeable, Exception)

data NonTerminal
  = S
  | A
  | A'
  deriving (Show, Eq)

data SyntaxTree
  = Branch NonTerminal [SyntaxTree]
  | Leaf Token
  deriving (Show, Eq)


throwSyntaxException :: String -> String -> IO a
throwSyntaxException expected actual = throwIO $
  SyntaxException $ "Expected " <> expected <> ", found " <> actual

assert :: (MonadIO m, Show b) => (b -> Bool) -> String -> b -> m ()
assert predicate expected actual = liftIO $ unless (predicate actual) $
  throwSyntaxException expected $ show actual

isName :: Token -> Bool
isName (NAME _) = True
isName _        = False

parseA' :: StateT Environment IO SyntaxTree
parseA' = do
  curToken <- nextToken
  case curToken of
    COMMA -> do
      name <- nextToken
      assert isName "argument name" name
      t <- parseA'
      return $ Branch A'
        [ Leaf curToken
        , Leaf name
        , t
        ]
    RPAREN -> do
      moveBack
      return $ Branch A' [Leaf EPS]
    _ -> liftIO $ throwSyntaxException (show COMMA <> " or " <> show RPAREN) (show curToken)

parseA :: StateT Environment IO SyntaxTree
parseA = do
  curToken <- nextToken
  case curToken of
    NAME _ -> do
      t <- parseA'
      return $ Branch A
        [ Leaf curToken
        , t
        ]
    RPAREN -> do
      moveBack
      return $ Branch A [Leaf EPS]
    _      -> liftIO $ throwSyntaxException ("argument name or " <> show RPAREN) (show curToken)


parse :: StateT Environment IO SyntaxTree
parse = do
  def <- nextToken
  assert (== DEF) (show DEF) def
  name <- nextToken
  assert isName "function name" name
  leftParen <- nextToken
  assert (== LPAREN) (show LPAREN) leftParen
  t <- parseA
  rightParen <- nextToken
  assert (== RPAREN) (show RPAREN) rightParen
  equals <- nextToken
  assert (== EQUALS) (show EQUALS) equals
  end <- nextToken
  assert (== END) (show END) end
  return $ Branch S
    [ Leaf def
    , Leaf name
    , Leaf leftParen
    , t
    , Leaf rightParen
    , Leaf equals
    ]
