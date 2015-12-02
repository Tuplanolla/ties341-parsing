{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Main where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.Char
import Data.List ((\\))
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Perm
import Text.Megaparsec.String

-- natural :: ParsecT String Identity _
-- natural :: Parsec String _
-- natural :: Parser _
natural =
  read <$> some digitChar
       <?> "natural number"

type PostalCode = ((Char, Int, Char), (Int, Char, Int))

postalCode =
  (,) <$> forwardSortationArea <* optional spaceChar <*> localDeliveryUnit
      <?> "postal code"
forwardSortationArea =
  (,,) <$> postalDistrict <*> region <*> section
       <?> "forward sortation area"
localDeliveryUnit =
  (,,) <$> digit <*> trailingLetter <*> digit
       <?> "local delivery unit"
postalDistrict = initialLetter <?> "postal district letter"
region = rural <|> urban <?> "region number"
section = trailingLetter <?> "section letter"
digit = digitToInt <$> digitChar
rural = digitToInt <$> char '0'
urban = digitToInt <$> oneOf ['1' .. '9']
trailingLetter = satisfy (`elem` ['A' .. 'Z'] \\ "DFIOQU")
initialLetter = notFollowedBy (oneOf "WZ") *> trailingLetter

testPostalCode :: String
testPostalCode = "P0N1E5"

data Listing a = Entry Int a | Group [Listing a]
  deriving (Read, Show)

listing p = entry p <|> group p <?> "group or entry"
group p =
  Group <$> (char '[' *> listing p `sepBy1` char ',' <* char ']')
        <?> "group"
entry p =
  Entry <$> natural <* char ':' <*> p
        <?> "entry"

testListing :: String
testListing = "[[1:P0N1E5],2:E4T1N6,[3:S7R0N6,4:T0X1N5]]"

data ListingT a = EntryT Int a | RefT Int | GroupT [ListingT a]
  deriving (Read, Show)

listingT p = label "group or entry" $
  entryT p <|> groupT p
groupT p = label "group" $
  do _ <- char '['
     xs <- listingT p `sepBy1` char ','
     _ <- char ']'
     return $ GroupT xs
entryT p = label "entry" $
  do i <- natural
     _ <- char ':'
     is <- get
     if i `elem` is then
        return $ RefT i else
        do x <- p
           put $ i : is
           return $ EntryT i x

testListingT :: String
testListingT = "[[1:P0N1E5],2:E4T1N6,[3:S7R0N6,1:,4:T0X1N5]]"

permissions = makePermParser flags <?> "permission flags"
flags =
  (,,) <$?> (False, True <$ char 'r')
       <|?> (False, True <$ char 'w')
       <|?> (False, True <$ char 'x')

testPermissions :: String
testPermissions = "xw"

expression = makeExprParser term operators <?> "expression"
term = between (char '(') (char ')') expression <|> natural <?> "term"
operators =
  [[unary '-' negate, unary '+' id],
   [binary '^' (^)],
   [binary '*' (*)],
   [binary '+' (+), binary '-' (-)]]
unary x f = Prefix $ f <$ char x
binary x f = InfixL $ f <$ char x

testExpression :: String
testExpression = "-1+2*3-(4+5*(-6))+(-7)"

main :: IO ()
main =
  do print $ runParser postalCode "" testPostalCode
     print $ runParser (listing postalCode) "" testListing
     print $ runParserT (listingT postalCode) "" testListingT `evalState` []
     print $ runParser permissions "" testPermissions
     print $ runParser expression "" testExpression
