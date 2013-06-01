module Snaplet.Search
    (search) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>),(*>),(<*))

import Snaplet.Types

data Search = StringMatch String
            | TagMatch String
            | AllOf [Search]
            | Not Search
    deriving(Show)


-- |Search for the matching documents,
-- return error string or documents, none
-- if none found.
search :: String -> [Doc] -> Either String [Doc]
search cond docs = do
    search <- parseSearch cond
    let p = buildMatcher search
    return $ filter p docs
    
buildMatcher :: Search -> Doc -> Bool
buildMatcher (StringMatch str) doc = match (manyTill anyChar (try (string str))) doc
buildMatcher (TagMatch tag)    doc = match (manyTill anyChar (try $ parseTag tag)) doc
buildMatcher (AllOf ses)       doc = all (\se -> buildMatcher se doc) ses
buildMatcher (Not s)           doc = not (buildMatcher s doc)

match p str = case parse p "" str of
    Left _  -> False
    Right _ -> True

hash = char '#'

parseTag tag = hash *> string tag

parseItem :: Parser Search
parseItem = spaces *> (not <|> parseTag <|> parseString) <* spaces
  where
    parseTag = do
      hash
      tag <- many1 alphaNum
      return $ TagMatch tag
    parseString = do
      str <- many1 alphaNum
      return $ StringMatch str
    not = Not <$> (char '-' *> parseItem)

parseSearch :: String -> Either String Search
parseSearch cond =
    case parse parseSearch' "" cond of
      Left p  -> Left $ show p
      Right x -> Right x
  where parseSearch' = AllOf <$> parseItem `sepBy1` spaces

