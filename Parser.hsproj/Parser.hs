{-# LANGUAGE FlexibleContexts #-}

import Text.ParserCombinators.Parsec

data JSON =
      B Bool
    | I Integer
    | F Float
    | S String
    | A [JSON]
    | O [(String,JSON)]
    deriving(Show)

wp :: Parser String
wp = many $ oneOf " "

lexeme :: Parser a -> Parser a
lexeme p = p <* wp

boolean :: Parser Bool
boolean = ( (string "True" <|> string "true") *> pure True ) 
    <|> ((string "False" <|> string "false") *> pure False ) <?> "boolean"

stringLiteral :: Parser String
stringLiteral = char '"' *> (many (noneOf "\"")) <* char '"'

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

number = many1 digit
plus = char '+' *> number
minus = char '-' <:> number
int = plus <|> minus <|> number

float = int <++> decimal <++> exponent
    where 
          decimal  = char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> int

integer = int <++> exponent
    where 
          exponent = option "" $ oneOf "eE" <:> integer


integerParser :: Parser Integer
integerParser =  fmap (read :: String -> Integer) integer 

floatParser :: Parser Float
floatParser =  fmap (read :: String -> Float) float

array :: Parser [JSON]
array = (lexeme $ char '[') 
            *> sepBy json_Value (lexeme $ char ',') <* 
        (lexeme $ char ']')
    
object_entry :: Parser (String,JSON)
object_entry = do
        key <- lexeme stringLiteral
        lexeme $ char ':'
        value <- lexeme json_Value
        return (key,value)

object :: Parser [(String,JSON)]
object = (lexeme $ char '{') 
            *> sepBy object_entry (lexeme $ char ',') <* 
        (lexeme $ char '}')

json_Bool :: Parser JSON
json_Bool = lexeme $ fmap B boolean

json_Integer :: Parser JSON
json_Integer =  lexeme $ fmap I integerParser

json_Float :: Parser JSON
json_Float =  lexeme $ fmap F floatParser

json_StringLiteral :: Parser JSON
json_StringLiteral = lexeme $ fmap S stringLiteral

json_Array :: Parser JSON
json_Array = lexeme $ fmap A array

json_Object :: Parser JSON
json_Object = lexeme $ fmap O object

json_Value :: Parser JSON
json_Value = json_Bool <|> try(json_Float) <|> try(json_Integer) <|> json_StringLiteral <|> json_Array <|> json_Object


json_to_string :: JSON -> String
json_to_string (B b) = show b 
json_to_string (A a) = '[' : ( listToString a ) ++ "]"
json_to_string (O o) = '{' :  (objectToString o) ++ "}"
json_to_string (I i) = show i
json_to_string (S s) = show s
json_to_string (F f) = show f


listToString :: [JSON] -> String
listToString [] = ""
listToString (x:[]) = (json_to_string x) 
listToString (x:xs) = (json_to_string x) ++ "," ++ (listToString xs)

objectToString :: [(String,JSON)] -> String
objectToString [] = ""
objectToString ((k,v):[]) = '"' : k ++ "\":" ++ (json_to_string v)
objectToString ((k,v):xs) = '"' : k ++ "\":" ++ (json_to_string v) ++ "," ++ (objectToString xs)
 
{-
parsing :: String -> IO [Char]
parsing string = do
    a <- case string_to_json string of
        Left e -> do putStrLn $ "Error parsing input:" ++ show e
                     return "err"
        Right r -> do
                   return $ toString r
    return a
-}

-----------
string_to_json :: String -> Either ParseError JSON
string_to_json = parse json_Value "(stdin)"




