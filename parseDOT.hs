import Control.Monad (mplus)
import Data.Char
import Control.Applicative ((<*>),(<$>),pure)
import Text.Parsec
import Text.Parsec.String (Parser)

data Graph = Graph String [Entity] | Digraph String [Entity] deriving Show
data Entity = Node String [Attrib] | DiEdge String String [Attrib] | Edge String String [Attrib] deriving Show
data Attrib = Attrib String String deriving Show
data Token = Token_id String | Token_str String | Token_arrow_r | Token_arrow_l | Token_arrow | Token_semi | Token_br_l | Token_br_r | Token_eq | Token_comma | Token_kw String deriving Show

----- graph level parser -----
parse_digraph = do
	blank
	token_keyword "digraph"
	blank
	name <- token_to_string <$> token_str
	blank
	token_keyword "{"
	e <- many $ parse_entity
	token_keyword "}"
	return $ Graph name e

----- entity parser ------

token_to_string (Token_str str) = str
token_to_string (Token_id str) = str

blank = many $ token_comment <|> (return <$> token_wsp)

parse_entity :: Parser Entity
parse_entity = do
	blank
	name <- token_str
	parse_nested_edge name <|> do
		a <- parseAttrib
		token_semi
		return $ Node (token_to_string name) a

parseAttrib :: Parser [Attrib]
parseAttrib = parseAttribList <|> return []
	where parseAttribList = do
		token_br_l
		let body = do
			name <- token_id
			token_eq
			val  <- token_str
			return $ Attrib (token_to_string name) (token_to_string val)
		w <- body `sepBy` token_comma
		token_br_r	
		return w

parse_nested_edge :: Token -> Parser Entity
parse_nested_edge name = eof >> (return $ Node "" []) -- always fail


----- TOKENS -------

token_br_l :: Parser Token
token_br_l = char '[' >> return Token_br_l
token_br_r :: Parser Token
token_br_r = char ']' >> return Token_br_r
token_semi :: Parser Token
token_semi = char ';' >> return Token_semi
token_eq :: Parser Token
token_eq   = char '=' >> return Token_eq
token_comma :: Parser Token
token_comma   = char ',' >> return Token_comma

token_wsp = oneOf " \t\r"

token_comment :: Parser String
token_comment = token_comment_line <|> token_comment_block

token_comment_line = do
	w1 <- string "//"
	w2 <- (many $ noneOf "\n")
	w3 <- string "\n"
	return $ w1 ++ w2 ++ w3

token_comment_block = do
	w1 <- string "/*" 
	w2 <- manyTill anyChar (try $ string "*/")
	return $ w1 ++ w2 ++ "*/"

token_id_str :: Parser String
token_id_str = (( (:)<$>(letter <|>char '_')) <*> many (alphaNum <|> char '_'))

token_quoted_str :: Parser String
token_quoted_str = do
	char '"'
	w <- concat <$> many ((pure <$> noneOf "\"")<|> string "\\\"")
	char '"'
	return w

token_keyword :: String -> Parser Token
token_keyword k = Token_kw <$> string k

token_id :: Parser Token
token_id = Token_id <$> token_id_str

token_str :: Parser Token
token_str = Token_str <$> (token_id_str <|> token_quoted_str)


