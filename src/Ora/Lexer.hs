module Ora.Lexer where
import qualified Text.Parsec as P
import Data.Char (isSpace)
import Control.Applicative ((<$>), (<*>))

data Token = TokenSymbol String
           | TokenInt Integer
           | TokenEof
           deriving Show

tokenize :: String -> [Token]
tokenize str = case P.parse parseOneToken "" str of
  Right (token@TokenEof, _) -> [token]
  Right (token, left) -> (token : tokenize left)
  Left e              -> error . ("[BUG]" ++) . show $ e

parseOneToken :: P.Parsec String u (Token, String)
parseOneToken = (,) <$> oneToken <*> (P.stateInput <$> P.getParserState)

oneToken :: P.Parsec String u Token
oneToken = do
  t <- int P.<|> op P.<|> symbol P.<|> (P.eof >> return TokenEof)
  P.spaces
  return t

opChars :: String
opChars = "()+*-/"

int :: P.Parsec String u Token
int = do
  x <- P.many1 $ P.oneOf "0123456789"
  return . TokenInt . read $ x

op :: P.Parsec String u Token
op = P.oneOf opChars >>= return . TokenSymbol . (: "")

symbol :: P.Parsec String u Token
symbol = do
  symbolString <- P.many1 (P.satisfy isSymbolChar)
  return . TokenSymbol $ symbolString
  where
    isSymbolChar c = (not . isSpace $ c) && (c `notElem` opChars)
