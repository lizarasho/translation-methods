import Control.Exception (try)
import Control.Monad.State (evalStateT)
import Data.Either (isLeft)
import LexicalAnalyzer (ParsingException (..), Token (..), initialEnvironment)
import Parser (NonTerminal (..), SyntaxException (..), SyntaxTree (..), parse)
import qualified System.IO.Streams as Streams (fromList)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [positiveTests, negativeTests]

generateTree :: String -> IO SyntaxTree
generateTree function = do
  inputStream <- Streams.fromList function
  let env = initialEnvironment inputStream
  evalStateT parse env

compareTrees :: String -> SyntaxTree -> Assertion
compareTrees function target = do
  actual <- generateTree function
  actual @?= target

positiveTests :: TestTree
positiveTests =
  testGroup
    "Positive tests"
    [ testCase "\"def f()=\": A -> ε" $ compareTrees "def f()=" $
        Branch S [Leaf DEF,Leaf (NAME "f"),Leaf LPAREN,Branch A [Leaf EPS],Leaf RPAREN,Leaf EQUALS]
    , testCase "\"def f(a)=\": A -> V A', A' -> ε" $ compareTrees "def f(a)=" $
        Branch S
          [ Leaf DEF
          , Leaf (NAME "f")
          , Leaf LPAREN,
            Branch A
              [ Leaf (NAME "a")
              , Branch A' [Leaf EPS]]
          , Leaf RPAREN
          , Leaf EQUALS ]
    , testCase "\"def f(a, b)=\": A -> V A', A' -> , V A' -> , V ε" $ compareTrees "def f(a, b)=" $
        Branch S
          [ Leaf DEF
          , Leaf (NAME "f")
          , Leaf LPAREN
          , Branch A [Leaf (NAME "a"),Branch A' [Leaf COMMA,Leaf (NAME "b"),Branch A' [Leaf EPS]]]
          , Leaf RPAREN
          , Leaf EQUALS ]
    , testCase "\"def f(a, b, c, d, e)=\": 5 arguments" $ compareTrees "def f(a, b, c, d, e)=" $
        Branch S
          [ Leaf DEF
          , Leaf (NAME "f")
          , Leaf LPAREN,
            Branch A
              [ Leaf (NAME "a")
              , Branch A' [Leaf COMMA,Leaf (NAME "b")
              , Branch A' [Leaf COMMA,Leaf (NAME "c")
              , Branch A' [Leaf COMMA,Leaf (NAME "d")
              , Branch A' [Leaf COMMA,Leaf (NAME "e")
              , Branch A' [Leaf EPS]]]]]]
          , Leaf RPAREN
          , Leaf EQUALS
          ]
    , testCase "\"def  \\t    f \\n  (  \\r  a  )=\": whitespace" $ compareTrees "def  \t    f \n  (  \r  a  )=" $
        Branch S
          [ Leaf DEF
          , Leaf (NAME "f")
          , Leaf LPAREN,
            Branch A
              [ Leaf (NAME "a")
              , Branch A' [Leaf EPS]]
          , Leaf RPAREN
          , Leaf EQUALS ]
    , testCase "\"def LONG_FuNcTiOn_name(a, b)=\": test function name" $
        compareTrees "def LONG_FuNcTiOn_name(a, b)=" $
        Branch S
          [ Leaf DEF
          , Leaf (NAME "LONG_FuNcTiOn_name")
          , Leaf LPAREN
          , Branch A [Leaf (NAME "a"),Branch A' [Leaf COMMA,Leaf (NAME "b"),Branch A' [Leaf EPS]]]
          , Leaf RPAREN
          , Leaf EQUALS ]
    , testCase "\"def _private(TeStVaR_A, _PRIVATE_b__variable__)=\": test arguments' names " $
        compareTrees "def _private(TeStVaR_A, _PRIVATE_b__variable__)=" $
        Branch S
          [ Leaf DEF
          , Leaf (NAME "_private")
          , Leaf LPAREN
          , Branch A
              [ Leaf (NAME "TeStVaR_A")
              , Branch A' [Leaf COMMA
              , Leaf (NAME "_PRIVATE_b__variable__")
              , Branch A' [Leaf EPS]]]
          , Leaf RPAREN
          , Leaf EQUALS ]
    ]

catchParsingException :: String -> Assertion
catchParsingException function  = do
  actual <- try (generateTree function) :: IO (Either ParsingException SyntaxTree)
  assertBool "Parsing exception was expected" $ isLeft actual

catchSyntaxException :: String -> Assertion
catchSyntaxException function  = do
  actual <- try (generateTree function) :: IO (Either SyntaxException SyntaxTree)
  assertBool "Syntax exception was expected" $ isLeft actual

negativeTests :: TestTree
negativeTests =
  testGroup
    "Negative tests"
    [ testCase "ParsingException test: \". def f(a, b, c, d, e)=\"" $
        catchParsingException ". def f(a, b, c, d, e)="
    , testCase "ParsingException test: \"def 1f(a, b, c, d, e)=\"" $
        catchParsingException "def 1f(a, b, c, d, e)="
    , testCase "ParsingException test: \"def ф(a, b, c, d, e)=\"" $
        catchParsingException "def ф(a, b, c, d, e)="
    , testCase "ParsingException test: \"def f(a, b,! c, d, e)=\"" $
        catchParsingException "def f(a, b,! c, d, e)="
    , testCase "ParsingException test: \"def f(~~)=\"" $
        catchParsingException "def f(~~)="
    , testCase "ParsingException test: \"def g(a,b,c,d,e,f,1l,g)=\"" $
        catchParsingException "def g(a,b,c,d,e,f,1l,g)="
    , testCase "ParsingException test: \"def g(a,b,c,d,e,f,g)=???\"" $
        catchParsingException "def g(a,b,c,d,e,f,g)=???"
    , testCase "ParsingException test: \"def valid(ыыы))))=\"" $
        catchParsingException "def valid(ыыы))))="
    , testCase "SyntaxException test: \"def_ f()=\"" $
        catchSyntaxException "def_ f()="
    , testCase "SyntaxException test: \"_def f()=\"" $
        catchSyntaxException "_def f()="
    , testCase "SyntaxException test: \"def def()=\"" $
        catchSyntaxException "def def()="
    , testCase "SyntaxException test: \"def f(def)=\"" $
        catchSyntaxException "def f(def)="
    , testCase "SyntaxException test: \"def def(a, b, c, def, e)=\"" $
        catchSyntaxException "def def(a, b, c, def, e)="
    , testCase "SyntaxException test: \"def f(a,b,c,)=\"" $
        catchSyntaxException "def def(a,b,c,)="
    , testCase "SyntaxException test: \"def f(,a,b)=\"" $
        catchSyntaxException "def def(,a,b)="
    , testCase "SyntaxException test: \"def f(()=\"" $
        catchSyntaxException "def def(()="
    , testCase "SyntaxException test: \"def f())=\"" $
        catchSyntaxException "def def())="
    , testCase "SyntaxException test: \"def f(),\"" $
        catchSyntaxException "def def(),"
    , testCase "SyntaxException test: \"def ,()=\"" $
        catchSyntaxException "def ,()="
    ]
