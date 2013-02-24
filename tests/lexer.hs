module Main (main) where
import Ora.Lexer
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT . TestCase $ do
    assertEqual "empty" [TokenEof] (tokenize "")
    assertEqual "expression" [
      TokenSymbol "("
      ,TokenInt 1
      ,TokenSymbol "+"
      ,TokenInt 2
      ,TokenSymbol ")"
      ,TokenSymbol "*"
      ,TokenInt 3
      ,TokenEof] (tokenize "(1+ 2) *3")
    assertEqual "expression" [
      TokenInt 1
      ,TokenSymbol "ac"
      ,TokenSymbol "+"
      ,TokenInt 2
      ,TokenSymbol ")"
      ,TokenEof] (tokenize "1ac+2)")
  return ()
