--------------------------------------------------------------------------------
-- Common parsers elements
--------------------------------------------------------------------------------
module Vandelay.DSL.Core.ParserT (
    doubleP,
    boolP,
    eol,
) where

import RIO hiding (try)
import Text.Parsec hiding (many, optional, (<|>))

-- Integers
intP ∷ (Stream s m Char) ⇒ ParsecT s u m Int
intP = (*) <$> option 1 (char '-' >> return (-1)) <*> unsignedIntP

boolP ∷ (Stream s m Char) ⇒ ParsecT s u m Bool
boolP =
    try (string "true" >> pure True)
        <|> try (string "True" >> pure True)
        <|> try (string "TRUE" >> pure True)
        <|> try (string "t" >> pure True)
        <|> try (string "T" >> pure True)
        <|> try (string "false" >> pure False)
        <|> try (string "False" >> pure False)
        <|> try (string "FALSE" >> pure False)
        <|> try (string "f" >> pure False)
        <|> try (string "F" >> pure False)
        <|> fail "Unable to parse boolean"

unsignedIntP ∷ (Stream s m Char) ⇒ ParsecT s u m Int
unsignedIntP =
    hoistMaybe "Error parsing integer"
        $ readMaybe
        <$> many1 digit

-- Doubles
doubleP ∷ (Stream s m Char) ⇒ ParsecT s u m Double
doubleP = parNegNumber <|> negativeNumber <|> unsignedExponentiatedNumber

parNegNumber ∷ (Stream s m Char) ⇒ ParsecT s u m Double
negativeNumber ∷ (Stream s m Char) ⇒ ParsecT s u m Double
parNegNumber = (0 -) <$> (char '(' *> unsignedExponentiatedNumber <* char ')')
negativeNumber = (0 -) <$> (char '-' *> unsignedExponentiatedNumber)

unsignedExponentiatedNumber ∷ (Stream s m Char) ⇒ ParsecT s u m Double
unsignedExponentiatedNumber = (*) <$> unsignedNumber <*> optionalExponent

unsignedNumber ∷ (Stream s m Char) ⇒ ParsecT s u m Double
unsignedNumber =
    hoistMaybe "Error parsing floating point"
        $ try (read3 <$> many1 digit <*> string "." <*> many1 digit)
        <|> try (read3 <$> many1 digit <*> string "." <*> return "0")
        <|> try (read3 <$> return "0" <*> string "." <*> many1 digit)
        <|> try (read3 <$> many1 digit <*> return "" <*> return "")
  where
    read3 ∷ String → String → String → Maybe Double
    read3 a b c = readMaybe (a ++ b ++ c)

hoistMaybe ∷ String → ParsecT s u m (Maybe a) → ParsecT s u m a
hoistMaybe errormsg pma =
    pma >>= \case
        Nothing → error errormsg
        Just a → return a

exponentialTerm ∷ (Stream s m Char) ⇒ ParsecT s u m Double
exponentialTerm = (10 ^^) <$> (oneOf "eE" >> intP)

optionalExponent ∷ (Stream s m Char) ⇒ ParsecT s u m Double
optionalExponent = option 1 exponentialTerm

-- End of line
eol ∷ (Stream s m Char) ⇒ ParsecT s u m String
eol =
    try (string "\n\r")
        <|> try (string "\r\n")
        <|> string "\n"
        <|> string "\r"
