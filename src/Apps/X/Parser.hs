{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Apps.X.Parser
  ( XCommand (..)
  , readX
  ) where

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Combinators
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Ratio

data XCommand
  = Create  Text
  | Adjust  Text Rational
  | ShowOne Text
  | ShowAll
  -- | LedgerQueryCmd LedgerQuery
  -- | CommandInputQuery RequestId
  deriving (Eq, Show)

xParser
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
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

create
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
create = (Right . Create . T.pack) <$ ssString "create" <*> some anyChar

adjust
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
adjust = do
  _ <- ssString "adjust"
  a <- manyTill anyChar space
  _ <- skipSpace
  amt <- myRational
  return $ Right $ Adjust (T.pack a) amt

showOne
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
showOne = (Right . ShowOne . T.pack) <$ ssString "showone" <*> some anyChar

showAll
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
showAll = do
  _ <- ssString "showall"
  return $ Right ShowAll

-- negative representation: -10%1, (-10)%1, ((-10)%1) (-10%1)
-- positive representation:  10%1, (10)%1, ((10)%1), (10%1)
myRational :: (Monad m, TokenParsing m) => m Rational
myRational = do
  _ <- optional $ char '('
  _ <- optional $ char '('
  sig <- optional $ char '-'
  n <- decimal
  _ <- optional $ char ')'
  _ <- ssChar '%'
  d <- decimal
  _ <- optional $ char ')'
  case sig of
    Nothing -> return (n % d)
    Just _ -> return ((-n) % d)

skipSpace :: TokenParsing m => m ()
skipSpace = skipMany space

ssChar :: (Monad m, TokenParsing m) => Char -> m ()
ssChar a = skipSpace >> char a >> skipSpace

ssString :: (Monad m, TokenParsing m) => String -> m ()
ssString a = skipSpace >> string a >> skipSpace

