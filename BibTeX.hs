{- |
The parsers in this module also skip trailing spaces.
-}
module BibTeX (
   file,
   comment,
   entry,
   assignment,
   value,
   texSequence,
   texBlock,
   identifier,
   bibIdentifier,

   -- utility functions
   skippingSpace,
   skippingLeadingSpace,

   splitCommaSepList,
   splitAuthorList,
   splitSepList,
   ) where

import qualified Text.BibTeX.Entry as Entry

import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
   (CharParser, Parser,
    (<|>), alphaNum, digit, letter, char, noneOf, oneOf,
    between, many, many1, sepEndBy, )

import Control.Monad (liftM, liftM2, liftM3, )

import Data.List.HT (chop, )
import Data.Maybe


lexer :: T.TokenParser st
lexer =
   T.makeTokenParser $ L.emptyDef {
      L.commentLine = "%",
      L.identStart = alphaNum,
      L.identLetter = alphaNum
   }


identifier, comma, equals :: CharParser st String
identifier = T.identifier lexer
comma = T.comma lexer
equals = T.symbol lexer "="

braces, lexeme :: CharParser st a -> CharParser st a
braces = T.braces lexer
lexeme = T.lexeme lexer

{- |
Beware that this and all other parsers do not accept leading spaces,
cf. 'skippingSpace'.
That is when encountering leading white spaces
the parser will just return an empty list.
If you want to parse a file that contains entirely of BibTeX data
you better call @skippingLeadingSpace file@ instead.
However, the @file@ parser is more combinable
and can be used for files that contain both BibTeX and other data
or it can be used for automated filetype checking.
-}
file :: Parser [Entry.T]
file = comment >> catMaybes <$> sepEndBy entry comment


comment :: Parser String
comment = many $ noneOf "@"


{- |
Parse a BibTeX entry like

> @article{author2010title,
>   author = {Firstname Surname},
>   title = {Title},
>   year = 2010,
>   month = jul,
> }

.
-}
entry :: Parser (Maybe Entry.T) -- could be onyl a comment
entry =
   do entryType <- char '@' >> identifier
      if entryType == "comment" then return Nothing
        else fmap Just $ braces $
             liftM2 (Entry.Cons entryType)
             (Parsec.try bibIdentifier)
             (comma >> sepEndBy assignment comma)

{- |
Parse an assignment like

> author = {Firstname Surname}

.
-}
assignment :: Parser (String, String)
assignment =
   liftM2 (,)
      bibIdentifier
      (equals >> value)

{- |
Parse a value like

> jul

or

> 2010

or

> {Firstname Surname}

or

> "Firstname Surname"

.
-}
value :: Parser String
value =
   lexeme (many1 letter) <|> -- for fields like: month = jul
   lexeme (many1 digit)  <|> -- for fields like: year = 2010
   braces (texSequence '}') <|>
   lexeme (between (char '"') (char '"') (texSequence '"'))

{- |
Parse a sequence of 'texBlock's until the occurrence of a closing character.
The closing character is not part of the result.
-}
texSequence :: Char -> Parser String
texSequence closeChar =
   liftM concat (many (texBlock closeChar))

{- |
Parse a single character like @a@,
a LaTeX macro call like @\\alpha@
or a block enclosed in curly braces like @{\\\"{a}bc}@.
-}
texBlock :: Char -> Parser String
texBlock closeChar =
   liftM3 (\open body close -> open : body ++ close : [])
      (char '{') (texSequence '}') (char '}') <|>
   sequence
      [char '\\',
       oneOf "_{}$|'`^&%\".,~# " <|> letter] <|>
   fmap (:[]) (noneOf [closeChar])


{- |
Parse a name of a BibTeX entry like @author2010title@.
-}
bibIdentifier :: Parser String
bibIdentifier =
   lexeme $
   liftM2 (:) (alphaNum <|> char '_')  -- less restrictive to handle all the internet shit!
   (many (alphaNum <|> oneOf "&;:-_.?+/"))


{- |
Extends a parser, such that all trailing spaces are skipped.
It might be more comfortable to skip all leading spaces,
but parser written that way are hard to combine.
This is so, since if you run two parsers in parallel
and both of them expect leading spaces,
then the parser combinator does not know
which one of the parallel parsers to choose.

See also: 'lexeme'.
-}
skippingSpace :: Parser a -> Parser a
skippingSpace p =
   do x <- p
      Parsec.skipMany Parsec.space
      return x

skippingLeadingSpace :: Parser a -> Parser a
skippingLeadingSpace p =
   Parsec.skipMany Parsec.space >> p


-- * Convert contents of BibTeX fields into lists

{- |
Split a string at the commas and remove leading spaces.
-}
splitCommaSepList :: String -> [String]
splitCommaSepList = splitSepList ','

{- |
Split a string containing a list of authors in BibTeX notation.
-}
splitAuthorList :: String -> [String]
splitAuthorList =
   map unwords . chop ("and" ==) . words

splitSepList :: Char -> String -> [String]
splitSepList sep =
   map (dropWhile (' '==)) . chop (sep==)
