module ExprParser (parseExprD,parseExprF,parseExprI,parseExprInt) where

import ExprType

import Text.Parsec
import Text.Parsec.String

{-Parse Exprs
    ---------------------
    Takes a string of format:
    and parses an expression of Expr + 
-}

-- * Parsers

-- ** Main Parsers


parseExprD :: String -- ^ The string to be parsed 
            -> Expr Double -- ^ The resulting parsed Double Expr
parseExprD ss = case parse dFactor "" "("++ss++")" of
                    Left err -> error (show err)
                    Right expr -> expr

parseExprF:: String  -- ^ The string to be parsed 
            -> Expr Float -- ^ The resulting parsed Float Expr
parseExprF ss = case parse fFactor "" "("++ss++")" of
                    Left err -> error (show err)
                    Right expr -> expr

parseExprI:: String -- ^ The string to be parsed
            -> Expr Integer -- ^ The resulting parsed Integer Expr
parseExprI ss = case parse iFactor "" "("++ss++")" of 
                    Left err -> error (show err)
                    Right expr -> expr

parseExprInt :: String -- ^ The string to be parsed
            -> Expr Int -- ^ The resulting parsed Int Expr
parseExprInt ss = case parse intFactor "" "("++ss++")" of
                    Left err -> error (show err)
                    Right expr -> expr

-- ** Main parser Helpers

iFactor :: Parser (Expr Integer)
iFactor = try (parens $ expr iFactor) <|> try iConst <|> var

iConst = do {i <- integer;
            return (Const i)}

intFactor :: Parser (Expr Int)
intFactor = try (parens $ expr intFactor) <|> try intConst <|> var

intConst = do {i <- int;
            return (Const i)}

dFactor :: Parser (Expr Double)
dFactor = try (parens $ expr dFactor) <|> try dConst <|> var

dConst = do {d <- double;
            return (Const d)}

fFactor :: Parser (Expr Float)
fFactor = try (parens $ expr fFactor) <|> try fConst <|> var

fConst = do {f <- float;
            return (Const f)}

-- ** Utility parsers
{-Common parsers, common to most of the parser stuffs-}

-- | attempt to parse an identifier for a variable consisting of alpha-numerical characters
iden :: Parser String
iden = many1 alphaNum

-- | attempt to parse a variable consisting of a potentially space padded identifier
var :: Parser (Expr a)
var = do {  spaces;
            ss <-iden;
            spaces;
            return (Var ss)}

-- | attempt to parse a multiplication operation consisting of the symbol !*
multOp :: Parser (Expr a -> Expr a -> Expr a)
multOp = do {symbol "!*";
            return (Mult)}

-- | attempt to parse an addition operation consisting of the symbol !+
addOp :: Parser (Expr a -> Expr a -> Expr a)
addOp = do {symbol "!+";
            return (Add)}

-- | attempt to parse an exponentiation operation consisting of the symbol !^
expOp :: Parser (Expr a -> Expr a -> Expr a)
expOp = do {symbol "!^";
            return (Exp)}

-- generic trig operation stuffs cause only diff = log, vs sin, vs cos etc
trigOp :: String ->
            (Expr a -> Expr a) ->
            Parser (Expr a) ->
            Parser (Expr a)
trigOp name val root = do {symbol name;
                            expr <- root;
                         return (val expr)}
-- | Attempt to parse an expression consisting of terms separated by addition operations
expr :: Parser (Expr a) -> Parser (Expr a)
expr factor = (term factor) `chainl1` addOp
-- | Attempt to parse a term consisting of trig expressions separated by multiplication operations
term :: Parser (Expr a) -> Parser (Expr a)
term factor = (expo factor) `chainl1` multOp

expo :: Parser (Expr a) -> Parser (Expr a)
expo factor = (trig factor) `chainl1` expOp
{- | Attemtpt to parse a trig operation which is any operation of the form `name(Expr...)` wherein name
    of the trig operation and Expr... is the nestled expressions within
-}
trig :: Parser (Expr a) -> Parser (Expr a)
trig factor = try (trigOp "cos" Cos factor) 
            <|> try (trigOp "sin" Sin factor) 
            <|> try (trigOp "ln" Ln factor) 
            <|> try (trigOp "e^" NExp factor)
            <|> factor


{- Utility parsers
    parens : surrounds parser in round braces
    symbol : parses some string that may be surrounded by spaces
    digits : parses a positive int or fails
    negDigits: parses a negative int or fails
    dec : parses the decimal aka numbers after the `.` including `.`
    mantissa : parses the number(+/-) after the `e` including `e`
    nilStr : A parser that simply returns the empty string ""
    decimal : parses a decimal in the general form `#.#e?#` # represenging any number of digits and ? indicating a potential position for `-`
    negDecimal : same as Decimal except in the form `-#.#e?#`
    float : parses a float of the form `?#.#e?#` # represenging any number of digits and ? indicating a potential position for `-`
    double : parses a double of the form `?#.#e?#` # represenging any number of digits and ? indicating a potential position for `-
    integer : parses a positive or negative integer or fails
    int : parses a number to a machine integer or fails

-}
-- ** Generic parsers

-- *** General utility

parens :: Parser a -> Parser a
parens p = do { symbol "(";
                cs <- p;
                symbol ")";
                return cs }


symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

-- *** Numeric Utility

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

dec :: Parser String
dec = do {
            char '.';
            a <- digits;
            return ("."++a)}

mantissa :: Parser String
mantissa = do {
            char 'e';
            e <- (try negDigits<|>digits);
            return ("e"++e);
            }

nilStr :: Parser String
nilStr = do {return ""}

decimal :: Parser String 
decimal = do {r <- digits;
                a <- (try dec<|>nilStr);
                e <- (try mantissa<|>nilStr);
            return (r++a++e)}

negDecimal :: Parser String
negDecimal = do {neg <- symbol "-";
                dec <- decimal;
            return (neg++dec)}

-- *** Number Parsers

float :: Parser Float
float = fmap read $ try negDecimal <|> decimal

double :: Parser Double
double = fmap read $ try negDecimal <|> decimal

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

int :: Parser Int
int = fmap read $ try negDigits <|> digits
