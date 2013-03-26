import Control.Monad
import Numeric (readHex, readOct, readFloat)
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal = Atom String
			 | List [LispVal]
			 | DottedList [LispVal] LispVal
			 | Number Integer
			 | String String
			 | Bool Bool
       | Character Char
       | Float Float

spaces :: Parser ()
spaces = skipMany1 space

parseEscapedQuote :: Parser Char
parseEscapedQuote = do 
                  char '\\'
                  c <- oneOf ['"', '\'', 'n', 'r', 't']
                  return $ case c of
                          '\\' -> c
                          '"' -> c
                          'n' -> '\n'
                          'r' -> '\r'
                          't' -> '\t' 


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (parseEscapedQuote <|> (noneOf ['"', '\\']))
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parseIntegral:: Parser LispVal
parseIntegral = do
              d <- many1 digit
              return $ (Number . read) d

parseFloat :: Parser LispVal
parseFloat = do
           w <- many1 digit
           char '.'
           f <- many1 digit
           return $ (Float . read) (w ++ "." ++ f)

parsePlainNumber :: Parser LispVal
parsePlainNumber = parseIntegral <|> parseFloat

parseDec :: Parser LispVal
parseDec = do
         char 'd'
         d <- oneOf ['0'..'9']
         return $ ((Number . read . show) d)

parseHex :: Parser LispVal
parseHex = do
         char 'x'
         h <- (oneOf ['0'..'9']) <|> (oneOf ['A'..'F']) <|> (oneOf ['a'..'f'])
         return $ ((Number . (parseReadValue readHex) . show) h)

parseOct :: Parser LispVal
parseOct = do
         char 'o'
         o <- oneOf ['0'..'8']
         return $ ((Number . (parseReadValue readOct) . show) o)

parseReadValue f s = fst $ f s !! 0

parseRadixNumber :: Parser LispVal
parseRadixNumber = do
                 char '#'
                 n <- (parseDec <|> parseHex <|> parseOct)
                 return n

parseQuasiquote :: Parser LispVal
parseQuasiquote = do char '`'
                     expr <- parseExpr
                     return $ List [Atom "quasiquote", expr]

parseUnquote :: Parser LispVal
parseUnquote = do char ','
                  expr <- parseExpr
                  return $ List [Atom "unquote", expr]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do string ",@"
                          expr <- parseExpr
                          return $ List [Atom "unquote-splicing", expr]

parseBool :: Parser LispVal
parseBool = do
          char '#'
          b <- oneOf ['f', 't']
          return $ case b of
              'f' -> Bool False
              't' -> Bool True

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> try parseBool
          <|> try parseChar
          <|> try parseFloat
          <|> try parseNumber
          <|> try parseUnquoteSplicing
          <|> parseQuoted
          <|> parseQuasiquote
          <|> parseUnquote
          <|> do char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x

parseChar :: Parser LispVal
parseChar = do
          char '#'
          char '\\'
          c <- letter <|> digit <|> symbol
          return $ Character c

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
     head <- endBy parseExpr spaces
     tail <- char '.' >> spaces >> parseExpr
     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match:" ++ show err
    Right val -> "Found Value" ++ show val

unwordsList :: [LispVal] -> String
unwordsList vars = (unwords (map showVal vars))

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

main :: IO()
main = do
	args <- getArgs
	putStrLn (readExpr(args !! 0))