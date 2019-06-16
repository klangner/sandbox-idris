module SimpleParser


-- ----------------------------------------------------------------------------
-- Helper functions
-- ----------------------------------------------------------------------------
charToInt : Char -> Maybe Int
charToInt c = if elem c (unpack "0123456789")
              then Just ((cast c) - (cast '0'))
              else Nothing


-- ----------------------------------------------------------------------------
-- Core types
-- ----------------------------------------------------------------------------

Token : Type
Token = Char

Tokens : Type
Tokens = List Token

||| Result type
data Result a = Error | Ok (a, Tokens)

Functor Result where
  map f Error = Error
  map f (Ok (a, input)) = Ok (f a, input)

filterResult : (a -> Bool) -> Result a -> Result a
filterResult f (Ok (x, input)) = if f x
                                 then Ok (x, input)
                                 else Error
filterResult _ Error = Error

filterMaybes : Result (Maybe a) -> Result a
filterMaybes (Ok (Just x, input)) = Ok (x, input)
filterMaybes _ = Error


||| Parser type
record Parser a where
    constructor MkParser
    runParser : Tokens -> Result a

Functor Parser where
  map f (MkParser p) = MkParser ((map f) . p)

Applicative Parser where
  pure a = MkParser (\input => Ok (a, input))

  (<*>) (MkParser f) (MkParser g)
      = MkParser (\input => case f input of
                              Error => Error
                              Ok (f', input') => map f' (g input'))

Alternative Parser where
    empty = MkParser (const Error)

    (<|>) p q = MkParser (\input => case (runParser p) input of
                                    Error => (runParser q) input
                                    ok => ok)

Monad Parser where
  -- m a -> ((result : a) -> m b) -> m b
  p >>= q = MkParser (\input => case (runParser p) input of
                                  Ok (x', input') => (runParser (q x')) input'
                                  err => Error)


-- ----------------------------------------------------------------------------
-- Parsers
-- ----------------------------------------------------------------------------


||| Parser will only succeed if the output passes the predicate
guard : (a -> Bool) -> Parser a -> Parser a
guard f p = MkParser $ filterResult f . runParser p

||| Parser will only succeed if the output can be converted to specific type
guardMaybe : (a -> Maybe b) -> Parser a -> Parser b
guardMaybe f p = MkParser $ filterMaybes . map f . runParser p


anyChar : Parser Char
anyChar = MkParser readChar
  where
    readChar [] = Error
    readChar (x::xs) = Ok (x, xs)


char : Char -> Parser Char
char c = guard (== c) anyChar


digit : Parser Int
digit = guardMaybe charToInt anyChar


-- ----------------------------------------------------------------------------
-- Combinators
-- ----------------------------------------------------------------------------
mutual

  same : Parser a -> Parser (List a)
  -- same p = (::) <$> p <*> many p
  -- same p = (::) <$> p <*> pure []
  same p = empty <*> same p


  many : Parser a -> Parser (List a)
  many p = same p <|> pure []

-- ----------------------------------------------------------------------------
-- Parse functions
-- ----------------------------------------------------------------------------

parse : Parser a -> Tokens -> Result a
parse (MkParser f) input = f input


parseStr : Parser a -> String -> Either String a
parseStr p input = case parse p (unpack input) of
                            Error => Left ("Error parsing " ++ input)
                            Ok (res, _) => Right res


parseString : Parser a -> String -> Either String (a, Tokens)
parseString p input = case parse p (unpack input) of
                            Error => Left ("Error parsing " ++ input)
                            Ok res => Right res

-- ----------------------------------------------------------------------------
-- same tests
-- ----------------------------------------------------------------------------
char2 : Parser Int
char2 = do d1 <- digit
           d2 <- digit
           pure (d1 + d2)
