{-# LANGUAGE OverloadedStrings #-}
module Handler.DSL.Parser where

import Import
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import qualified Data.Text as T

ws :: Parser String
ws = many (oneOf " ")


int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit


stringLike :: Parser String
stringLike = char '"' *> many (noneOf ['\"', '\r', '\n']) <* char '"'


-- A parser combinator which skips whitespaces from both sides
lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws


alwaysTrue :: Parser Bool
alwaysTrue = pure True

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q


-- Here we are saying, try to match one between
-- gr and ml, if you can't default to Nothing.
-- The trick is using  pure :: a -> f a
measureP :: Parser (Maybe Text)
measureP = (string "gr" *> (pure . Just $ "gr"))
       <|> (string "ml" *> (pure . Just $ "ml"))
       <|> (string "spoon" *> (pure . Just $ "spoon"))
       <|> (string "cup" *> (pure . Just $ "cup"))
       <|> (pure Nothing)


syntacticSugar :: String -> Parser (Maybe String)
syntacticSugar s = (string s *> (pure . Just $ s)) <|> pure Nothing


ingredient :: Parser Ingredient
ingredient = do
    qt <- lexeme int
    ms <- lexeme measureP
    lexeme (syntacticSugar "of")
    name <- lexeme stringLike
    lexeme (syntacticSugar "and")
    string "\r\n"
    return $ Ingredient (T.pack name) qt ms

-- Step
-------------------------------------------------------------------------------
step :: Parser Step
step = do
    sn <- lexeme stringLike
    d <- optionMaybe durationP
    lexeme (syntacticSugar "and")
    string "\r\n" <||> pure ""
    return $ Step (T.pack sn) 1 d

-- Duration
-------------------------------------------------------------------------------
durationP :: Parser Duration
durationP = do
    lexeme (string "for")
    d <- lexeme int
    u <- lexeme durationUnit
    return $ Duration d (T.pack u)
  where durationUnit = string "seconds" <|> string "minutes" <|> string "hours"


-- Recipe
-------------------------------------------------------------------------------
recipe :: Parser Recipe
recipe = do
    rn <- lexeme stringLike
    lexeme (syntacticSugar "is made with") *> string "\r\n"
    i <- many1 ingredient
    many1 (string "\r\n")
    lexeme (string "prepared by") *> string "\r\n"
    s <- many1 step
    return $ Recipe (T.pack rn) i s