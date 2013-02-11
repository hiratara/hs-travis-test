module Ora.Lexer where
import qualified Text.Parsec as P

data Token = TokenSymbol String
           | TokenInt Integer deriving Show

tokenize :: String -> [Either P.ParseError [Token]]
tokenize "" = []
tokenize str = case P.parse parseLine "" str of
  Right (tokens, left) -> (return tokens : tokenize left)
  error@(Left e)       -> [error >>= return . fst]

parseLine :: P.Parsec String () ([Token], String)
parseLine = do
  line <- tokens
  state <- P.getParserState
  return (line, P.stateInput state)

tokens :: P.Parsec String () [Token]
tokens = do
  tokenList <- P.many token
  _ <- (P.newline >> return ()) P.<|> P.eof
  return tokenList
  where
    token = do
      t <- int P.<|> op P.<|> symbol
      _ <- P.many $ P.satisfy (`elem` spaceChars)
      return t

opChars, spaceChars :: String
spaceChars = " \t\r"
opChars = "()+*-/"

int :: P.Parsec String () Token
int = do
  x <- P.many1 $ P.oneOf "0123456789"
  return . TokenInt . read $ x

op :: P.Parsec String () Token
op = P.oneOf opChars >>= return . TokenSymbol . (: "")

symbol :: P.Parsec String () Token
symbol = do
  symbolString <- P.many1 (P.satisfy (`notElem` '\n' : spaceChars ++ opChars))
  return . TokenSymbol $ symbolString
