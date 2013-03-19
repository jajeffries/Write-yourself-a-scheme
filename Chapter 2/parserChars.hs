import Control.Monad
import Numeric (readHex, readOct)
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

parsePlainNumber :: Parser LispVal
parsePlainNumber = liftM (Number . read) $ many1 digit

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

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseChar

parseChar :: Parser LispVal
parseChar = do
          char '#'
          char '\\'
          c <- letter <|> digit <|> symbol
          return $ Character c

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match:" ++ show err
    Right val -> "Found Value"

main :: IO()
main = do
	args <- getArgs
	putStrLn (readExpr(args !! 0))