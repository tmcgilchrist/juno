{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Apps.X.Parser
  ( XCommand (..)
  , readX
  ) where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8            as BSC
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

data XCommand
  = Create  Text
  | Adjust  Text Integer
  | ShowOne Text
  | ShowAll
  deriving (Eq, Show)

xParser :: (Monad m, TokenParsing m) => m (Either String XCommand)
xParser
  =   create
  <|> adjust
  <|> showOne
  <|> showAll

readX :: BSC.ByteString -> Either String XCommand
readX m = case Atto.parseOnly xParser m of
  Left e          -> Left $ "Syntax Error: " ++ e
  Right (Left e)  -> Left $ "Language Error: " ++ e
  Right (Right v) -> Right v

create :: (Monad m, TokenParsing m) => m (Either String XCommand)
create = (Right . Create . T.pack) <$ ssString "create" <*> some anyChar

adjust :: (Monad m, TokenParsing m) => m (Either String XCommand)
adjust = do
  _   <- ssString "adjust"
  a   <- manyTill anyChar space
  _   <- skipSpace
  amt <- decimal
  return $ Right $ Adjust (T.pack a) amt

showOne :: (Monad m, TokenParsing m) => m (Either String XCommand)
showOne = (Right . ShowOne . T.pack) <$ ssString "showone" <*> some anyChar

showAll :: (Monad m, TokenParsing m) => m (Either String XCommand)
showAll = do
  _ <- ssString "showall"
  return $ Right ShowAll

skipSpace :: TokenParsing m => m ()
skipSpace = skipMany space

ssString :: (Monad m, TokenParsing m) => String -> m ()
ssString a = skipSpace >> string a >> skipSpace

