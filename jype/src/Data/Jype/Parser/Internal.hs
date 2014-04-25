module Data.Jype.Parser.Internal
    ( decls
    , decl
    ) where

import Control.Applicative
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parser.Char (CharParsing, char, oneOf, noneOf, letter, alphaNum, newline)
import Text.Parser.Combinators (sepBy, sepBy1, (<?>))
import Text.Parser.Token (TokenParsing, symbol, token, stringLiteral, integer, brackets, braces)

import Data.Jype.Syntax

decls :: TokenParsing m => m [Decl]
decls = many decl

decl :: TokenParsing m => m Decl
decl = (\d t b -> Decl t b d)
    <$> many (token desc)
    <*> (name <* symbol "=")
    <*> body

desc :: TokenParsing m => m Text
desc = T.pack <$> (char '#' *> spaceTabs *> notNewline <* newline)

name :: TokenParsing m => m TypeName
name = TypeName <$> identifier <*> (asum <$> optional params)
  where
    params = brackets $ identifier `sepBy` symbol ","

identifier :: TokenParsing m => m String
identifier = (:) <$> letter <*> many alphaNum <?> "identifier"

body :: TokenParsing m => m Body
body = object <|> choices

object :: TokenParsing m => m Body
object = braces $ Object <$> many field

field :: TokenParsing m => m Field
field = token $ (\ds k t d -> Field k t ds d)
    <$> many (token desc)
    <*> (identifier <* symbol ":")
    <*> concreteType
    <*> optional (spaceTabs *> desc)

concreteType :: TokenParsing m => m ConcreteType
concreteType = ConcreteType <$> identifier <*> (asum <$> optional params)
  where
    params = brackets $ concreteType `sepBy` symbol ","

choices :: TokenParsing m => m Body
choices = Choice <$> choice `sepBy1` symbol "|"

choice :: TokenParsing m => m (Either Value ConcreteType)
choice = Left <$> value <|> Right <$> concreteType

value :: TokenParsing m => m Value
value = NullValue <$ symbol "null"
    <|> BoolValue True <$ symbol "true"
    <|> BoolValue False <$ symbol "false"
    <|> StringValue <$> stringLiteral
    <|> IntValue <$> integer

spaceTabs :: CharParsing m => m String
spaceTabs = many (oneOf " \t")

notNewline :: CharParsing m => m String
notNewline = many (noneOf "\n")
