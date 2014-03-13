module IdeSlave(parseSExp, convSExp, SExp(..)) where

import Control.Applicative ((<$>), (<$), (*>))
import Numeric (readDec)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Printf (printf)

data SExp = SexpList [SExp]
          | StringAtom String
          | BoolAtom Bool
          | IntegerAtom Integer
          | SymbolAtom String
          deriving ( Eq, Show )

sExpToString :: SExp -> String
sExpToString (StringAtom s)   = "\"" ++ escape s ++ "\""
sExpToString (BoolAtom True)  = ":True"
sExpToString (BoolAtom False) = ":False"
sExpToString (IntegerAtom i)  = printf "%d" i
sExpToString (SymbolAtom s)   = ':' : s
sExpToString (SexpList l)     = "(" ++ unwords (map sExpToString l) ++ ")"

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = [c]

pSExp :: Parser SExp
pSExp = SexpList <$> between (char '(') (char ')') (pSExp `sepBy` char ' ')
    <|> atom

atom :: Parser SExp
atom = char ':' *> atomC
   <|> StringAtom <$> between (char '"') (char '"') (many quotedChar)
   <|> do ints <- many1 digit
          case readDec ints of
            ((num, ""):_) -> return (IntegerAtom num)
            _ -> return (StringAtom ints)

atomC :: Parser SExp
atomC = BoolAtom True <$ string "True"
    <|> BoolAtom False <$ string "False"
    <|> SymbolAtom <$> many (noneOf " \n\t\r\"()")

quotedChar :: Parser Char
quotedChar = try (string "\\\\" >> return '\\')
         <|> try (string "\\\"" >> return '"')
         <|> noneOf "\""

parseSExp :: String -> Either ParseError SExp
parseSExp = parse pSExp "(unknown)"

convSExp :: String -> String -> Integer -> String
convSExp pre s ident =
  let sex = SexpList [SexpList [SymbolAtom pre, StringAtom s], IntegerAtom ident] in
      let str = sExpToString sex in
          getHexLength str ++ str

getHexLength :: String -> String
getHexLength s = printf "%06x" (1 + length s)
