module ExprParser (parseExprD,parseExprF,parseExprI) where

import ExprType

import Text.Parsec
import Text.Parsec.String

{-Parse Exprs
    ---------------------
    Takes a string of format:
    and parses an expression of Expr + 
-}
{- | Parses an expression to Expr double
    using the parsec package
-}
parseExprD :: String -- ^ The string to be parsed 
            -> Expr Double -- ^ The resulting parsed double Expr
parseExprD ss = case parse dFactor "" ss of
                    Left err -> error (show err)
                    Right expr -> expr

parseExprF:: String -> Expr Float
parseExprF ss = case parse fFactor "" ss of
                    Left err -> error (show err)
                    Right expr -> expr

parseExprI:: String -> Expr Integer
parseExprI ss = case parse iFactor "" ss of 
                    Left err -> error (show err)
                    Right expr -> expr

iFactor :: Parser (Expr Integer)
iFactor = try (parens $ expr iFactor) <|> try iConst <|> var

iConst = do {i <- integer;
            return (Const i)}

dFactor :: Parser (Expr Double)
dFactor = try (parens $ expr dFactor) <|> try dConst <|> var

dConst = do {d <- double;
            return (Const d)}

fFactor :: Parser (Expr Float)
fFactor = try (parens $ expr fFactor) <|> try fConst <|> var

fConst = do {f <- float;
            return (Const f)}
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

-- generic trig operation stuffs cause only diff = log, vs sin, vs cos etc
trigOp :: String ->
            (Expr a -> Expr a) ->
            Parser (Expr a) ->
            Parser (Expr a)
trigOp name val root = do {symbol name;
                            expr <- root;
                         return (val expr)}

expr :: Parser (Expr a) -> Parser (Expr a)
expr factor = (term factor) `chainl1` addOp

term :: Parser (Expr a) -> Parser (Expr a)
term factor = (trig factor) `chainl1` multOp

trig :: Parser (Expr a) -> Parser (Expr a)
trig factor = try (trigOp "cos" Cos factor) 
            <|> try (trigOp "sin" Sin factor) 
            <|> try (trigOp "log" Log factor) 
            <|> factor


{- Utility parsers
    parens : surrounds parser in round braces
    symbol : parses some string that may be surrounded by spaces
    digits : parses a positive int or fails
    negDigits: parses a negative int or fails
    integer : parses a positive or negative int or fails

-}

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

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

decimal :: Parser String 
decimal = do {r <- digits;
                char '.';
                a <- digits;
            return (r++"."++a)}

negDecimal :: Parser String
negDecimal = do {neg <- symbol "-";
                r <- digits;
                char '.';
                a <- digits;
            return (neg++r++"."++a)}

float :: Parser Float
float = fmap read $ try negDecimal <|> decimal

double :: Parser Double
double = fmap read $ try negDecimal <|> decimal

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

