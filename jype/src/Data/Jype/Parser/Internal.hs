module Data.Jype.Parser.Internal
    ( decls
    , decl
    ) where

import Control.Applicative
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parser.Char (CharParsing, char, oneOf, noneOf, letter, alphaNum, newline, spaces)
import Text.Parser.Combinators (sepBy, sepBy1, (<?>), eof, between, try)
import Text.Parser.Token (TokenParsing, symbol, token, stringLiteral, integer, brackets, braces)

import Data.Jype.Syntax

decls :: TokenParsing m => m [Decl]
decls = spaces *> many decl <* eof

decl :: TokenParsing m => m Decl
decl = token $ (\d t b -> Decl t b d)
    <$> many (token desc)
    <*> (token name <* symbol "=")
    <*> body

desc :: TokenParsing m => m Text
desc = T.pack <$> (char '#' *> spaceTabs *> notNewline <* newline)

name :: TokenParsing m => m TypeName
name = TypeName <$> token identifier <*> (asum <$> optional params)
  where
    params = brackets $ token identifier `sepBy` symbol ","

identifier :: TokenParsing m => m String
identifier = (:) <$> letter <*> many (alphaNum <|> oneOf "'_") <?> "identifier"

body :: TokenParsing m => m Body
body = object <|> choices

object :: TokenParsing m => m Body
object = braces $ Object <$> many field

field :: TokenParsing m => m Field
field = token $ (\ds k t d -> Field k t ds d)
    <$> many (token desc)
    <*> token identifier <* symbol ":"
    <*> concreteType <* spaceTabs
    <*> optional desc

concreteType :: TokenParsing m => m ConcreteType
concreteType = try (ConcreteType <$> token identifier <*> params)
    <|> ConcreteType <$> identifier <*> pure []
  where
    params = between (symbol "[") (char ']') $ token concreteType `sepBy1` symbol ","

choices :: TokenParsing m => m Body
choices = Choice <$> token choice `sepBy1` symbol "|"

value :: TokenParsing m => m Value
value = (NullValue <$ symbol "null" <?> "null")
    <|> (BoolValue True <$ symbol "true" <?> "true")
    <|> (BoolValue False <$ symbol "false" <?> "false")
    <|> StringValue <$> stringLiteral
    <|> IntValue <$> integer

spaceTabs :: CharParsing m => m String
spaceTabs = many (oneOf " \t")

notNewline :: CharParsing m => m String
notNewline = many (noneOf "\n")

choice :: TokenParsing m => m Choice
choice = (\ds c d -> TypeChoice c ds d)
    <$> many (token desc)
    <*> choice'
    <*> optional desc
  where
    choice' = Left <$> value <|> Right <$> concreteType
