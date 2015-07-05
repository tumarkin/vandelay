{-# LANGUAGE FlexibleContexts #-}

module App.Vandelay.Shared.ParserT
  ( int
  , unsignedInt

  , double
  ) where

import Control.Applicative 
import Text.Parsec hiding (many, optional, (<|>))


-- Integers
int :: Stream s m Char => ParsecT s u m Int
int = (*) <$> option 1 (char '-' >> return (-1)) <*> unsignedInt

unsignedInt :: Stream s m Char => ParsecT s u m Int
unsignedInt = read <$> many1 digit



-- Doubles
double :: Stream s m Char => ParsecT s u m Double
double = parNegNumber <|> negativeNumber <|> unsignedExponentiatedNumber

parNegNumber   :: Stream s m Char => ParsecT s u m Double
negativeNumber :: Stream s m Char => ParsecT s u m Double

parNegNumber   = (0-) <$> (char '(' *> unsignedExponentiatedNumber <* char ')')
negativeNumber = (0-) <$> (char '-' *> unsignedExponentiatedNumber)


unsignedExponentiatedNumber :: Stream s m Char => ParsecT s u m Double
unsignedExponentiatedNumber = (*) <$> unsignedNumber <*> optionalExponent 


unsignedNumber :: Stream s m Char => ParsecT s u m Double
unsignedNumber =  
      try (read3 <$> many1 digit <*> string "." <*> many1 digit) 
  <|> try (read3 <$> many1 digit <*> string "." <*> return "0" ) 
  <|> try (read3 <$> return "0"  <*> string "." <*> many1 digit) 
  <|> try (read3 <$> many1 digit <*> return ""  <*> return ""  ) 
    where 
  read3 a b c = read (a++b++c)

exponentialTerm :: Stream s m Char => ParsecT s u m Double
exponentialTerm = (10^^) <$> (oneOf "eE" >> int)

optionalExponent :: Stream s m Char => ParsecT s u m Double
optionalExponent  = option 1 exponentialTerm


