import Data.List
import Text.Parsec hiding (tokens)
import Data.Maybe

type S = String

-- A parameter can be denoted by a name, a number, or one of the special
-- characters listed in Special Parameters.
--
-- A variable is a parameter denoted by a name.
--
-- A positional parameter is a parameter denoted by the decimal value
-- represented by one or more digits, other than the single digit 0.
data Parameter = Var String
               | Positional Int
               | Special Char
    deriving Show

data SubstringType = Suffix | Prefix
    deriving Show
data PatternType = Largest | Smallest
    deriving Show
data CheckType = CheckUnset | CheckUnsetAndNull
    deriving Show

data ParModifier = UseDefault       Word CheckType
                 | AssignDefault    Word CheckType
                 | Assert           Word CheckType
                 | UseAlternative   Word CheckType
                 | StringLength
                 | Remove           Word PatternType SubstringType
                 | NoModifier
    deriving Show

data ParSubstExpr = ParSubstExpr Parameter ParModifier
    deriving Show

data WordPart = Bare String
               | SQuoted String
               | DQuoted [WordPart]
               | Escaped Char
               | CommandSubst [WordPart]
               | ParSubst ParSubstExpr
               | Arith [WordPart]
    deriving Show

type Word = [WordPart]
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

bareWord :: String -> Parsec S u WordPart
bareWord terminators = do
    word <- many1 ordinarySymbol
    return $ Bare word
    where
    ordinarySymbol = noneOf terminators

escaped :: Parsec S u WordPart
escaped = do
    char '\\'
    c <- anyChar
    return $ Escaped c

doubleQuoted :: Parsec S u WordPart
doubleQuoted = do
    dQuote
    parts <- many1 $ escaped <|> bareWord "$`\\\"" <|> substitution
    dQuote
    return $ DQuoted parts
    where
    dQuote = char '"'
    escaped = do
        char '\\'
        fmap Escaped $ oneOf "$`\"\\\n"

word :: String -> Bool -> Parsec S u Word
word terminators acceptEmpty = do
    (if acceptEmpty then many else many1) $
        escaped <|> singleQuoted <|> doubleQuoted <|> substitution <|> bareWord terminators

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
-- Parameter expansion, command substitution or arithmetic expansion
substitution :: Parsec S u WordPart
substitution = do
    char '$'
    fmap ParSubst parameterSubst

-- A word consisting solely of underscores, digits, and alphabetics from the
-- portable character set. The first character of a name is not a digit. 
name :: Parsec S u String
name = do
    first <- underscore <|> letter
    rest <- many $ underscore <|> letter <|> digit
    return $ first:rest
    where
    -- portable character set
    letter = satisfy $ \x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')
    underscore = char '_'

parameterSubst :: Parsec S u ParSubstExpr
parameterSubst = do
    braced <|> unbraced <?> "parameter substitution"
    where
    lbrace = char '{'
    rbrace = char '}'

    braced = between lbrace rbrace $
        try string_length <|> 
        try parameter_check <|> 
        try parameter_substr <|>
        try (flip ParSubstExpr NoModifier `fmap` parameter)

    parameter = do special <|> positional <|> fmap Var name 

    word_arg = word "}'\"`$\\" True

    parameter_check = do
        par <- parameter
        colon <- optionMaybe $ char ':'
        op <- oneOf "-+=?"
        w <- word_arg
        
        let mod = case op of
             '-' -> UseDefault
             '=' -> AssignDefault
             '+' -> UseAlternative
             '?' -> Assert
        
        let checkType = if isJust colon then CheckUnsetAndNull else CheckUnset

        return $ ParSubstExpr par (mod w checkType)

    parameter_substr = do
        par <- parameter
        op <- choice $ map (try.string) ["%%","%","##","#"]
        w <- word_arg

        let mod = case op of
             "%"  -> Remove w Smallest Suffix
             "%%" -> Remove w Largest Suffix
             "#"  -> Remove w Smallest Prefix
             "##" -> Remove w Largest Prefix
        return $ ParSubstExpr par mod

    string_length = do
        char '#'
        par <- parameter
        return $ ParSubstExpr par StringLength

    unbraced = do
        par <- special -- should be the first to capture 0
           <|> simple_positional
           <|> fmap Var name
        return $ ParSubstExpr par NoModifier
    
    variable = fmap Var name <?> "variable"

    simple_positional = do
        d <- digit
        return $ Positional $ read [d]

    positional = do
        n <- many1 digit
        return $ Positional $ read n
    
    special = fmap Special $ oneOf "@*#?-$!0"

--- Tokens ---
tokens :: Parsec S u [Token]
tokens = do
    optional whiteSpace
    sepEndBy (token_word <|> operator) (optional whiteSpace)
    where token_word = fmap Word (word ("'\"`$\\\n# " ++ opFirstLetters) False)

main = do
    s <- getContents
    print $ parse tokens "" s
