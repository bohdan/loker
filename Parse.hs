import Data.List
import Data.Maybe
import Control.Monad
import Parsec hiding (token,tokens)

-- A parameter can be denoted by a name, a number, or one of the special
-- characters listed in Special Parameters.
--
-- A variable is a parameter denoted by a name.
--
-- A positional parameter is a parameter denoted by the decimal value
-- represented by one or more digits, other than the single digit 0.
type Name = String
data Parameter = Var Name
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
               | CommandSubst CompoundList
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

data Assignment = Assignment Name Word
    deriving Show

data Command = ComSimple SimpleCommand
             | ComCompound CompoundCommand 
             | ComFunction FunctionDefinition
    deriving Show
data FunctionDefinition =
	FunctionDefinition Name CompoundCommand [Redirection]
    deriving Show
data SimpleCommand = SimpleCommand [Assignment] [Redirection] [Word]
    deriving Show
data ForList = ForWords [Word] | ForPositional
    deriving Show
data CompoundCommand = BraceGroup CompoundList
                     | SubShell CompoundList
                     | For Name ForList CompoundList
                     | Case Word [([Word],CompoundList)]
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

singleQuoted :: Parser WordPart
singleQuoted = dontSkipLineConts $ do
    squote
    text <- many nonQuote
    squote
    return $ SQuoted text
    where
    squote = char '\''
    nonQuote = satisfy (/= '\'')

bareWord :: String -> Parser WordPart
bareWord terminators = do
    word <- many1 ordinarySymbol
    return $ Bare word
    where
    ordinarySymbol = noneOf terminators

escaped :: Parser WordPart
escaped = do
    char '\\'
    c <- anyChar
    return $ Escaped c

doubleQuoted :: Parser WordPart
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

word :: String -> Bool -> Parser Word
word terminators acceptEmpty = do
    (if acceptEmpty then many else many1) $
        escaped <|> singleQuoted <|> doubleQuoted <|> substitution <|> bareWord terminators

--- Operators ---

operator :: Parser String
operator = token $ do
    choice $ map (try.string) operatorList

theOperator op = try $ do
    op' <- operator
    guard $ op' == op
    return op

operatorList = ["(",")","&&","||",";;","<<",">>","<&",">&","<>","<<-",">|","<",">","&",";","|","\n"]
opFirstLetters = nub $ map head $ operatorList

--- Comments ---

comment :: Parser ()
comment = do
    char '#'
    many $ satisfy (/= '\n')
    return ()

whiteSpace :: Parser ()
whiteSpace = do many1 $ comment <|> (oneOf " \t" >> return ());
                return ()

--- Substitutions ---
-- Parameter expansion, command substitution or arithmetic expansion
substitution :: Parser WordPart
substitution = do
    char '$'
    fmap ParSubst parameterSubst <|> fmap CommandSubst commandSubst

-- A word consisting solely of underscores, digits, and alphabetics from the
-- portable character set. The first character of a name is not a digit. 
name :: Parser Name
name = token $ do
    first <- underscore <|> letter
    rest <- many $ underscore <|> letter <|> digit
    return $ first:rest
    where
    -- portable character set
    letter = satisfy $ \x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')
    underscore = char '_'

parameterSubst :: Parser ParSubstExpr
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

commandSubst :: Parser CompoundList
commandSubst = between (char '(') (char ')') compoundList

--- Tokens ---

token :: Parser a -> Parser a
token p = do optional whiteSpace; x <- p; optional whiteSpace; return x

separated p = do
    optional whiteSpace
    sepEndBy p (optional whiteSpace)

separated1 p = do
    optional whiteSpace
    sepEndBy1 p (optional whiteSpace)

{-
tokens :: Parser [Token]
tokens = do
    optional whiteSpace
    sepEndBy (fmap Word token_word <|> operator) (optional whiteSpace)
-}

token_word = token $ word ("'\"`$\\\n# " ++ opFirstLetters) False

--- Syntax ---
redirOp :: Parser RedirectionOp
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

redirection :: Parser Redirection
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

ifNotReserved :: Parser a -> Parser a
ifNotReserved p = try $ do
    r <- optionMaybe reservedWord
    case r of
	Just _ -> parserFail "unexpected reserved word"
	Nothing -> p

simpleCommand = ifNotReserved $ do
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
theReservedWord w = (<?> "reserved word \"" ++ w ++ "\"") $ try $ do
    w' <- reservedWord
    guard $ w == w'
    return w

linebreak = separated $ char '\n'
newline_list = separated1 $ char '\n'
sequential_sep = (theOperator ";" >> linebreak) <|> newline_list

pipeline = do
    bang <- do optionMaybe (theReservedWord "!")
    ps <- pipe_sequence
    let status = if isJust bang then Inverted else Straight
    return $ Pipeline status ps
    where
    pipe_sequence = do
        sepBy1 command (do theOperator "|"; linebreak)

functionDefinition :: Parser FunctionDefinition
functionDefinition = ifNotReserved $ do
    fname <- name
    string "()"
    linebreak
    body <- compoundCommand
    redirect <- many redirection
    return $ FunctionDefinition fname body redirect

command = try $ fmap ComFunction functionDefinition
      <|> fmap ComCompound compoundCommand
      <|> fmap ComSimple simpleCommand
      

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

list :: Parser CompoundList
list = do
    linebreak
    aol <- andOrList
    mode <- optionMaybe $
        (do theOperator ";"; linebreak; return Seq)   <|>
        (do theOperator "&"; linebreak; return Async) <|>
        (do newline_list;               return Seq)
    -- if there is separator, try to parse list further
    -- if there is no, finish
    case mode of
        Just mode -> do
                rest <- optionMaybe list
                return $ (aol,mode) : (concat.maybeToList) rest
        Nothing -> return [(aol,Seq)]

compoundList = list

compoundCommand = choice $
    [ braceGroup
    , subShell
    , forClause
    , ifClause
    , whileClause
    , untilClause
    , caseClause
    ]

braceGroup = fmap BraceGroup $ between (theReservedWord "{") (theReservedWord "}") compoundList

subShell = fmap SubShell $ between (theOperator "(") (theOperator ")") compoundList

doGroup = between (theReservedWord "do") (theReservedWord "done") compoundList

forClause = do
    theReservedWord "for"
    var <- name
    linebreak
    words <- optionMaybe wordlist
    do_group <- doGroup
    let list = case words of
         Just ws -> ForWords ws
         Nothing -> ForPositional
    return $ For var list do_group
    where
    wordlist = do
        theReservedWord "in"
        ws <- many token_word
        sequential_sep
        return ws

whileClause = do
    theReservedWord "while"
    l <- compoundList
    cmds <- doGroup
    return $ While l cmds

untilClause = do
    theReservedWord "until"
    l <- compoundList
    cmds <- doGroup
    return $ Until l cmds

ifClause = do
    theReservedWord "if"
    cond <- compoundList
    theReservedWord "then"
    then_part <- compoundList
    elif_parts <- many elif_part
    mb_else_part <- optionMaybe else_part
    theReservedWord "fi"
    return $ If ((cond,then_part):elif_parts) mb_else_part

    where
    elif_part = do
        theReservedWord "elif"
        cond <- compoundList
        theReservedWord "then"
        then_part <- compoundList
        return (cond, then_part)

    else_part = do
        theReservedWord "else"
        compoundList

caseClause = do
    theReservedWord "case"
    w <- token_word
    linebreak
    theReservedWord "in"
    linebreak
    cl <- case_list

    return $ Case w cl
    
    where
    -- case_item:
    --   returns Left () if it parsed "esac",
    --   returns Right (item, True) if it has parsed item with DSEMI
    --   returns Right (item, False) if it has parsed item without DSEMI
    pattern :: Parser [Word]
    pattern = sepBy1 token_word (char '|')

    case_item :: Parser (Either () (([Word],CompoundList), Bool))
    case_item = do
        openParen <- optionMaybe (theOperator "(")
        let esac = case openParen of
              -- if there was opening paren, don't recognise esac as reserved word
              Nothing -> do theReservedWord "esac"; return $ Left ()
              Just _ -> parserFail ""

            item = do
                pat <- pattern
                theOperator ")"
                linebreak
                cl <- optionMaybe compoundList
                dsemi <- optionMaybe (theOperator ";;")
                linebreak
                return $ Right ((pat,concat $ maybeToList cl),isJust dsemi)

        esac <|> item

    case_list = do
        ci <- case_item
        case ci of
            Left _ -> return []
            Right (pat,True) -> fmap (pat:) case_list
            Right (pat,False) -> do theReservedWord "esac"; return [pat]

main = do
    s <- getContents
    --print $ parse tokens "" s
    --print $ parse simpleCommand "" s
    --print $ parse andOrList "" s
    --print $ parse functionDefinition "" s
    print $ parse (do l <- list; eof; return l) "" s

--- Misc ---

number = do
    n <- many1 digit
    return $ read n
