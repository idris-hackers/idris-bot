module IdeSlave(parseSExp, convSExp, SExp(..)) where

import Text.Printf
import Numeric
import Data.List
import Text.Parsec

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
sExpToString (SymbolAtom s)   = ":" ++ s
sExpToString (SexpList l)     = "(" ++ intercalate " " (map sExpToString l) ++ ")"

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = [c]

pSExp = do xs <- between (char '(') (char ')') (pSExp `sepBy` (char ' '))
           return (SexpList xs)
    <|> atom

atom = do char ':'; x <- atomC; return x
   <|> do char '"'; xs <- many quotedChar; char '"'; return (StringAtom xs)
   <|> do ints <- many1 digit
          case readDec ints of
            ((num, ""):_) -> return (IntegerAtom (toInteger num))
            _ -> return (StringAtom ints)

atomC = do string "True"; return (BoolAtom True)
    <|> do string "False"; return (BoolAtom False)
    <|> do xs <- many (noneOf " \n\t\r\"()"); return (SymbolAtom xs)

quotedChar = try (string "\\\\" >> return '\\')
         <|> try (string "\\\"" >> return '"')
         <|> noneOf "\""

parseSExp :: String -> Either ParseError SExp
parseSExp = parse pSExp "(unknown)"

convSExp :: String -> String -> Integer -> String
convSExp pre s id =
  let sex = SexpList [SexpList [SymbolAtom pre, StringAtom s], IntegerAtom id] in
      let str = sExpToString sex in
          (getHexLength str) ++ str

getHexLength :: String -> String
getHexLength s = printf "%06x" (1 + (length s))
