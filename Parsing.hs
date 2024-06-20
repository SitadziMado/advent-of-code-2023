module Parsing where

import Text.Parsec.Char (digit, spaces, string)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (try)
import Text.Parsec.Text (Parser)

lexeme :: Parser a -> Parser a
lexeme m = m <* spaces

int :: Parser Int
int = read <$> many1 digit

string' :: String -> Parser String
string' = try . string
