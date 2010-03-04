module Parsec ( module Orig, module Parsec ) where
import Text.Parsec as Orig hiding (char,string,Stream,parse,satisfy,oneOf,noneOf)
import qualified Text.Parsec as Base (char,string,satisfy)
import Control.Monad.Reader

type Stream = String

data RS = RS
    { skipLineContinuation :: Bool }

type Parser = ParsecT Stream () (Reader RS)

lineConts :: Parser ()
lineConts = do 
    many $ try $ Base.string "\\\n"
    return ()

dontSkipLineConts p = do
    -- skip line conts before, do not skip inside
    lineConts
    local dontSkip p
    where
    dontSkip rs = rs { skipLineContinuation = False }

-- if skipLineContinuation is True, line continuation will be skipped /before/
-- the char
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = try $ do
    skiplc <- asks skipLineContinuation
    if skiplc
        then do lineConts; Base.satisfy f
        else Base.satisfy f

-- if skipLineContinuation is True, line continuation will be skipped /before/
-- the char
char :: Char -> Parser Char
char x = try $ do
    skiplc <- asks skipLineContinuation
    if skiplc
        then do lineConts; Base.char x
        else Base.char x
    
-- if skipLineContinuation is True, line continuation will be skipped before and
-- inside the string
string :: String -> Parser String
string s = try $ do
    sequence $ map char s
    return s

parse :: Parser a -> SourceName -> Stream -> Either ParseError a
parse p name s = runReader (runPT p () name s) RS { skipLineContinuation = True }

oneOf cs  = try $ satisfy (\c -> elem c cs)
noneOf cs = try $ satisfy (\c -> not (elem c cs))

