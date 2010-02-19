import Data.List
import Text.Parsec hiding (tokens)
import Control.Applicative ((<$>))

type S = String

data WordPart = Bare String
               | SQuoted String
               | DQuoted [WordPart]
               | Escaped Char
               | CommandSubst [WordPart]
               | Parameter [WordPart]
               | Arith [WordPart]
    deriving Show

data Token = Word [WordPart]
           | Op String
    deriving Show

singleQuoted :: Parsec S u WordPart
singleQuoted = do
    squote
    text <- many nonQuote
    squote
    return $ SQuoted text
    where
    squote = char '\''
    nonQuote = satisfy (/= '\'')

bareWord :: Parsec S u WordPart
bareWord = do
    word <- many1 ordinarySymbol
    return $ Bare word
    where
    ordinarySymbol = noneOf $ "'\"`$\\\n# " ++ opFirstLetters

escaped :: Parsec S u WordPart
escaped = do
    char '\\'
    c <- anyChar
    return $ Escaped c

doubleQuoted :: Parsec S u WordPart
doubleQuoted = do
    dQuote
    parts <- many1 $ escaped <|> bareWord <|> substitution
    dQuote
    return $ DQuoted parts
    where
    dQuote = char '"'
    escaped = do
        char '\\'
        fmap Escaped $ oneOf "$`\"\\\n"
    bareWord = fmap Bare $ (many1 $ noneOf "$`\\\"")

word :: Parsec S u Token
word = do
    parts <- many1 $ escaped <|> singleQuoted <|> doubleQuoted <|> substitution <|> bareWord
    return $ Word parts

--- Operators ---

operator :: Parsec S u Token
operator = do
    fmap Op $ choice $ map (try.string) operatorList

operatorList = ["&&","||",";;","<<",">>","<&",">&","<>","<<-",">|","<",">","&",";","|","\n"]
opFirstLetters = nub $ map head $ operatorList

--- Comments ---

comment :: Parsec S u ()
comment = do
    char '#'
    many $ satisfy (/= '\n')
    return ()

whiteSpace :: Parsec S u ()
whiteSpace = do many1 $ comment <|> (char ' ' >> return ());
                return ()

--- Substitutions ---
substitution :: Parsec S u WordPart
substitution = do
    char '$'
    parameter

parameter :: Parsec S u WordPart
parameter = do
    fmap Parameter $ braced <|> unbraced
    where
    lbrace = char '{'
    rbrace = char '}'
    braced = do
        lbrace
        expr <- many (escaped <|> singleQuoted <|> doubleQuoted <|> bare)
        rbrace
        return expr
        where
        escaped = do char '\\'; fmap Escaped $ char '}'
        bare = fmap Bare $ many1 $ satisfy (/= '}')

    unbraced = fmap (:[]) $ simple_positional <|> name
    simple_positional = fmap (Bare.(:[])) digit
    name = fmap Bare $ many1 letter --XXX precise name
        
    

--- Tokens ---
tokens :: Parsec S u [Token]
tokens = do
    optional whiteSpace
    sepEndBy (word <|> operator) (optional whiteSpace)

main = do
    s <- getContents
    print $ parse tokens "" s
