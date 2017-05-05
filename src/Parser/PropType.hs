{-# LANGUAGE OverloadedStrings #-}

module Parser.PropType where

import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Text            (Text, pack)
import           Options.Applicative
import           Types
import           Types.PropTypes

optparseProps :: ReadM [Prop]
optparseProps = eitherReader (A.parseOnly (parseCLIPropTypes <* A.endOfInput) . pack)

parseCLIPropTypes :: A.Parser [Prop]
parseCLIPropTypes = parseProp `A.sepBy1'` A.space

parseProp :: A.Parser Prop
parseProp = do
  propName <- liftA pack $ A.many' (A.letter <|> A.digit)
  A.string ":"
  A.skipSpace
  propType <- parsePropType
  required <- maybeParser parseRequiredStatus
  return $ case required of
    Nothing ->
      Prop propName propType Optional
    Just _ ->
      Prop propName propType Required

maybeParser :: A.Parser a -> A.Parser (Maybe a)
maybeParser p = A.option Nothing (Just <$> p)

parseRequiredStatus :: A.Parser IsRequired
parseRequiredStatus = do
  requiredStatus <- A.string ".isRequired"
  return Required

parsePropType :: A.Parser PropType
parsePropType =
      parseInstanceOf
  <|> parseOneOf
  <|> parseShape
  <|> parseOneOfType
  <|> parseArrayOf
  <|> parseObjectOf
  <|> (A.string "any"     >> return Any)
  <|> (A.string "array"   >> return Array)
  <|> (A.string "bool"    >> return Bool)
  <|> (A.string "func"    >> return Func)
  <|> (A.string "number"  >> return Number)
  <|> (A.string "object"  >> return Object)
  <|> (A.string "string"  >> return String)
  <|> (A.string "symbol"  >> return Symbol)
  <|> (A.string "node"    >> return Node)
  <|> (A.string "element" >> return Element)

parseInstanceOf :: A.Parser PropType
parseInstanceOf = do
  A.string "instanceOf("
  instanceOf <- A.takeTill (==')')
  A.char ')'
  return $ InstanceOf instanceOf

parseArrayOf :: A.Parser PropType
parseArrayOf = do
  A.string "arrayOf("
  arrayOf <- parsePropType
  A.char ')'
  return $ ArrayOf arrayOf

parseObjectOf :: A.Parser PropType
parseObjectOf = do
  A.string "objectOf("
  objectOf <- parsePropType
  A.char ')'
  return $ ObjectOf objectOf

parseOneOfType :: A.Parser PropType
parseOneOfType = do
  A.string "oneOfType(["
  types <- commaSeparated parsePropType
  A.string "])"
  return $ OneOfType types

parseOneOf :: A.Parser PropType
parseOneOf = do
  A.string "oneOf(["
  items <-  commaSeparated $ A.many' (A.letter <|> A.digit)
  A.string "])"
  return $ OneOf (pack <$> items)

parseShape :: A.Parser PropType
parseShape = do
  A.string "shape({"
  keyValuePairs <- commaSeparated parseProp
  A.string "})"
  return $ Shape keyValuePairs

commaSeparated :: A.Parser a -> A.Parser [a]
commaSeparated p = p `A.sepBy` (A.string ", " <|> A.string ",")
