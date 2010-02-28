import Data.List
import Text.Parsec hiding (token,tokens)
import Data.Maybe
import Control.Monad

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

data Redirection = Redirection Int RedirectionOp Word
    deriving Show

data StripHereDoc = Strip | NoStrip
    deriving Show
data Clobber = Clobber | NoClobber
    deriving Show
data RedirectionOp = RedirectInput
                   | RedirectOutput Clobber
                   | AppendOutput
                   | HereDoc StripHereDoc
                   | DupInput
                   | DupOutput
                   | ReadWrite
    deriving Show

data Assignment = Assignment String Word
    deriving Show

data Command = ComSimple SimpleCommand
             | ComCompound CompoundCommand 
--           | ComFunction FunctionDefinition
    deriving Show
data SimpleCommand = SimpleCommand [Assignment] [Redirection] [Word]
    deriving Show
data CompoundCommand = BraceGroup CompoundList
                     | SubShell CompoundList
                     | For String [Word] CompoundList
--                   | Case TODO
                     | If [(CompoundList,CompoundList)] -- 'if' and 'elif'
                           (Maybe CompoundList) -- optional 'else'
                     | While CompoundList CompoundList
                     | Until CompoundList CompoundList
    deriving Show
data PipelineStatus = Straight | Inverted
    deriving Show
data Pipeline = Pipeline PipelineStatus [Command]
    deriving Show
data AndOrList = First Pipeline
               | And   Pipeline AndOrList
               | Or    Pipeline AndOrList
    deriving Show
data ExecutionMode = Seq | Async
    deriving Show
type CompoundList = [(AndOrList,ExecutionMode)]

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
    parts <- many1 $ escaped <|> bare_word <|> substitution
    dQuote
    return $ DQuoted parts
    where
    dQuote = char '"'
    escapables = "$`\"\\\n"
    escaped = try $ do
        char '\\'
        fmap Escaped $ oneOf escapables
    bare_word = do
        w <- many1 ordinary_symbol
        return $ Bare w
        where
        ordinary_symbol = noneOf "$`\\\"" <|>
            do char '\\'; lookAhead (noneOf escapables); return '\\'

word :: String -> Bool -> Parsec S u Word
word terminators acceptEmpty = do
    (if acceptEmpty then many else many1) $
        escaped <|> singleQuoted <|> doubleQuoted <|> substitution <|> bareWord terminators

--- Operators ---

operator :: Parsec S u String
operator = token $ do
    choice $ map (try.string) operatorList

theOperator op = try $ do
    op' <- operator
    guard $ op' == op
    return op

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
name = token $ do
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

    positional = fmap Positional number
    
    special = fmap Special $ oneOf "@*#?-$!0"

--- Tokens ---

token :: Parsec S u a -> Parsec S u a
token p = do optional whiteSpace; x <- p; optional whiteSpace; return x

separated p = do
    optional whiteSpace
    sepEndBy p (optional whiteSpace)

separated1 p = do
    optional whiteSpace
    sepEndBy1 p (optional whiteSpace)

{-
tokens :: Parsec S u [Token]
tokens = do
    optional whiteSpace
    sepEndBy (fmap Word token_word <|> operator) (optional whiteSpace)
-}

token_word = token $ word ("'\"`$\\\n# " ++ opFirstLetters) False

--- Syntax ---
redirOp :: Parsec S u RedirectionOp
redirOp = do
    op <- choice $ map (try.string) ["<<-",">>","<&",">&","<>","<<",">|","<",">"]
    return $ case op of
        "<"  -> RedirectInput
        ">"  -> RedirectOutput NoClobber
        ">|" -> RedirectOutput Clobber
        "<<" -> HereDoc NoStrip
        "<<-"-> HereDoc Strip
        ">>" -> AppendOutput
        "<&" -> DupInput
        ">&" -> DupOutput
        "<>" -> ReadWrite

redirection :: Parsec S u Redirection
redirection = do
    mbFd <- optionMaybe number
    op <- redirOp
    w <- token_word

    let fd = case mbFd of
            Just fd -> fd
            Nothing -> case op of
                RedirectOutput _ -> 1
                AppendOutput     -> 1
                DupOutput        -> 1
                RedirectInput    -> 0
                HereDoc _        -> 0
                DupInput         -> 0
                ReadWrite        -> 0

    return $ Redirection fd op w

assignment = do
    var <- name
    char '='
    value <- token_word
    return $ Assignment var value

simpleCommand = do 
    cmd_prefix <- separated (fmap add_assignment (try assignment) <|> fmap add_redirection redirection)
    cmd_word <- fmap maybeToList $ optionMaybe $ fmap add_word token_word
    cmd_suffix <- separated (try (fmap add_redirection redirection) <|> fmap add_word token_word)

    let (as,rs,ws) = foldr ($) ([],[],[]) (cmd_prefix ++ cmd_word ++ cmd_suffix)

    case (as,rs,ws) of
        ([],[],[]) -> parserFail "Empty command"
        _ -> return $ SimpleCommand as rs ws

    where
    add_assignment  a (as,rs,ws) = (a:as,rs,ws)
    add_redirection r (as,rs,ws) = (as,r:rs,ws)
    add_word        w (as,rs,ws) = (as,rs,w:ws)

reservedWords = ["!",  "{", "}", "case", "do", "done", "elif", "else", "esac",
                 "fi", "for", "if", "in", "then", "until", "while"]

reservedWord = try $ do
    [Bare x] <- token_word
    guard $ x `elem` reservedWords
    return $ x
theReservedWord w = do
    w' <- try reservedWord
    guard $ w == w'
    return w

linebreak = separated $ char '\n'
newline_list = separated1 $ char '\n'

pipeline = do
    bang <- do optionMaybe (theReservedWord "!")
    ps <- pipe_sequence
    let status = if isJust bang then Inverted else Straight
    return $ Pipeline status ps
    where
    pipe_sequence = do
        sepBy1 command (do theOperator "|"; linebreak)

command = fmap ComSimple $ simpleCommand

andOrList = do
    p <- pipeline
    op <- optionMaybe $ theOperator "&&" <|> theOperator "||"
    case op of
        Nothing -> return $ First p
        Just op -> do
            linebreak
            rest <- andOrList
            let opCon = case op of
                 "&&" -> And
                 "||" -> Or
            return $ opCon p rest

andOrListSep = do
    aol <- andOrList
    sep <- optionMaybe (theOperator ";" <|> theOperator "&")
    let mode = case sep of
            Nothing  -> Seq
            Just ";" -> Seq
            Just "&" -> Async
    return (aol,mode)

compoundList = do
    aols <- many1 andOrListSep
    linebreak
    return aols

main = do
    s <- getContents
    --print $ parse tokens "" s
    --print $ parse simpleCommand "" s
    --print $ parse andOrList "" s
    print $ parse compoundList "" s

--- Misc ---

number = do
    n <- many1 digit
    return $ read n
