import Control.Monad (mplus)
import Data.Char
import Control.Applicative ((<*>),(<$>),pure)
import Text.Parsec
import Text.Parsec.String (Parser)

data Graph = Graph String [Entity] | Digraph String [Entity] deriving Show
data Entity = Node String [Attrib] | DiEdge String String [Attrib] | Edge String String [Attrib] deriving Show
data Attrib = Attrib String String deriving Show
data Token = Token_id String | Token_str String | Token_arrow_r | Token_arrow_l | Token_arrow
           | Token_semi | Token_eq | Token_comma | Token_kw String
           | Token_br_l | Token_br_r | Token_bl_l | Token_bl_r
           deriving Show

----- graph level parser -----
parse_digraph = do
	blank -- skips preceding blanks
	token_keyword "digraph"
	name <- token_to_string <$> token_str
	token_bl_l
	e <- concat <$> (many $ parse_entity)
	token_bl_r
	return $ Graph name e

----- entity parser ------

parse_entity :: Parser [Entity]
parse_entity = do
	name <- token_to_string <$> token_str
	parse_nested_edge name

parse_nested_edge :: String -> Parser [Entity]
parse_nested_edge name = do
	r <- parse_nested_edge_sub name []
	token_semi
	return r

parse_nested_edge_sub :: String -> [(String,String)] -> Parser [Entity]
parse_nested_edge_sub name list = go <|> stop
	where
		go = do
			token_arrow_r
			name2 <- token_to_string <$> token_str
			parse_nested_edge_sub name2 ((name,name2):list)
		stop | list == [] = do
			a <- parseAttrib
			return $ [Node name a]
		     | otherwise = do
			a <- parseAttrib
			let edges = map (\(x, y) -> DiEdge x y a) list
			return $ edges

parseAttrib :: Parser [Attrib]
parseAttrib = parseAttribList <|> return []
	where parseAttribList = do
		token_br_l
		let body = do
			name <- token_to_string <$> token_id
			token_eq
			val  <- token_to_string <$> token_str
			return $ Attrib name val
		w <- body `sepBy` token_comma
		token_br_r	
		return w

----- TOKENS -------

token_to_string (Token_str str) = str
token_to_string (Token_id str) = str

blank :: Parser [String]
blank = many $ token_comment <|> (return <$> token_wsp)

greedy_blank :: Parser a -> Parser a
greedy_blank x = do
	v <- x
	blank
	return v

token_br_l :: Parser Token
token_br_l = greedy_blank $ char '[' >> return Token_br_l
token_br_r :: Parser Token
token_br_r = greedy_blank $ char ']' >> return Token_br_r
token_bl_l :: Parser Token
token_bl_l = greedy_blank $ char '{' >> return Token_br_l
token_bl_r :: Parser Token
token_bl_r = greedy_blank $ char '}' >> return Token_br_r
token_semi :: Parser Token
token_semi = greedy_blank $ char ';' >> return Token_semi
token_eq :: Parser Token
token_eq   = greedy_blank $ char '=' >> return Token_eq
token_comma :: Parser Token
token_comma   = greedy_blank $ char ',' >> return Token_comma
token_arrow_r :: Parser Token
token_arrow_r = greedy_blank $ string "->" >> return Token_br_r
token_arrow_l :: Parser Token
token_arrow_l = greedy_blank $ string "<-" >> return Token_br_r
token_arrow :: Parser Token
token_arrow = greedy_blank $ string "--" >> return Token_br_r

token_wsp :: Parser Char
token_wsp = oneOf " \t\r\n"

token_comment :: Parser String
token_comment = token_comment_line <|> token_comment_block

token_comment_line :: Parser String
token_comment_line = do
	w1 <- string "//"
	w2 <- (many $ noneOf "\n")
	w3 <- string "\n"
	return $ w1 ++ w2 ++ w3

token_comment_block :: Parser String
token_comment_block = do
	w1 <- string "/*" 
	w2 <- manyTill anyChar (try $ string "*/")
	return $ w1 ++ w2 ++ "*/"

token_id_str :: Parser String
token_id_str = greedy_blank $ (( (:)<$>(letter <|>char '_')) <*> many (alphaNum <|> char '_'))

token_quoted_str :: Parser String
token_quoted_str = do
	char '"'
	w <- concat <$> many ((pure <$> noneOf "\"")<|> string "\\\"")
	char '"'
	greedy_blank $ return w

token_keyword :: String -> Parser Token
token_keyword k = greedy_blank $ Token_kw <$> string k

token_id :: Parser Token
token_id = Token_id <$> token_id_str

token_str :: Parser Token
token_str = Token_str <$> (token_id_str <|> token_quoted_str)


