{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module App.X.Parser
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
  = CreateAccount Text
  | AdjustAccount Text Rational
  | ObserveAccount Text
  | ObserveAccounts
  -- | LedgerQueryCmd LedgerQuery
  -- | CommandInputQuery RequestId
  deriving (Eq, Show)

xParser
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
xParser
  =   createAccount
  <|> adjustAccount
  <|> observeAccount
  <|> observeAccounts

readX :: BSC.ByteString -> Either String XCommand
readX m = case Atto.parseOnly xParser m of
  Left e          -> Left $ "Syntax Error: " ++ e
  Right (Left e)  -> Left $ "Language Error: " ++ e
  Right (Right v) -> Right v

createAccount
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
createAccount = (Right . CreateAccount . T.pack) <$ ssString "CreateAccount" <*> some anyChar

adjustAccount
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
adjustAccount = do
  _ <- ssString "AdjustAccount"
  a <- manyTill anyChar space
  _ <- skipSpace
  amt <- myRational
  return $ Right $ AdjustAccount (T.pack a) amt

observeAccount
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
observeAccount = (Right . ObserveAccount . T.pack) <$ ssString "ObserveAccount" <*> some anyChar

observeAccounts
  :: (Monad m, TokenParsing m)
  => m (Either String XCommand)
observeAccounts = do
  _ <- ssString "ObserveAccounts"
  return $ Right ObserveAccounts

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

