{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import qualified Data.Attoparsec.Text as A
import           Parser.PropType
import           Test.HUnit
import           Types.PropTypes      as P

testParseInstanceOf = do
  let parsedValue = A.parseOnly parseInstanceOf "instanceOf(Date)"
  assertEqual "success - instanceOf(Date)" (Right $ InstanceOf "Date") parsedValue
  let failedParsedValue = A.parseOnly parseInstanceOf "instanceof(Date)"
  assertEqual "failure - instanceof(Date)" (Left "string") failedParsedValue

testParseArrayOf = do
  let parsedValue = A.parseOnly parseArrayOf "arrayOf(string)"
  assertEqual "success - arrayOf(string)" (Right $ ArrayOf P.String) parsedValue
  let failedParsedValue = A.parseOnly parseArrayOf "arrayof(string)"
  assertEqual "failure - arrayof(string)" (Left "string") failedParsedValue

testParseObjectOf = do
  let parsedValue = A.parseOnly parseObjectOf "objectOf(bool)"
  assertEqual "success - objectOf(string)" (Right $ ObjectOf P.Bool) parsedValue
  let failedParsedValue = A.parseOnly parseObjectOf "objectof(string)"
  assertEqual "failure - objectof(string)" (Left "string") failedParsedValue

testParseOneOfType = do
  let parsedValue = A.parseOnly parseOneOfType "oneOfType([object, number, func])"
  assertEqual "success - oneOfType([string, number, func])" (Right $ OneOfType [Object,Number,Func]) parsedValue
  let missingBrackets = A.parseOnly parseOneOfType "oneOfType(string, noop, number)"
  assertEqual "failure - missing brackets; oneOfType(string, noop, number)" (Left "string") missingBrackets
  let failedParsedValue = A.parseOnly parseOneOfType "oneOfType(string, noop, number)"
  assertEqual "failure - bad type; oneOfType([string, noop, number])" (Left "string") failedParsedValue

testParseOneOf = do
  let parsedValue = A.parseOnly parseOneOf "oneOf([red, blue, green])"
  assertEqual "success - oneOf([red, blue, green])" (Right $ OneOf ["red", "blue", "green"]) parsedValue
  let missingBrackets = A.parseOnly parseOneOf "oneOf(red, blue, green)"
  assertEqual "failure - missing brackets; oneOf(red, blue, green)" (Left "string") missingBrackets

testParseShape = do
  let parsedValue = A.parseOnly parseShape "shape({name: string.isRequired, age: number, dob: instanceOf(Date)})"
  let shape =
        Shape [
          Prop {_name = "name", _propType = P.String, _required = Required}
        , Prop {_name = "age", _propType = P.Number, _required = Optional}
        , Prop {_name = "dob", _propType = P.InstanceOf "Date", _required = Optional}
        ]
  assertEqual "success - shape({...})" (Right $ shape) parsedValue
  let malformedString = A.parseOnly parseShape "shapeOf({name: string.isRequired, age: number, dob: instanceOf(Date)})"
  assertEqual "failure - malformed string; shape({...})" (Left "string") malformedString
