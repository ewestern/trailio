{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.ByteString (Parser, word8, (<?>))
import qualified Data.Text as T
import Data.Text.Read
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Maybe
import Data.Word
import Data.Int
import qualified Database.PostgreSQL.Simple.FromField as F
import Database.PostgreSQL.Simple.Types  (PGArray)
import Data.ByteString.Lazy (fromStrict)

import Database.Postgis.Geometry
import Database.Postgis.Serialize

import Geo
import Types
import Debug.Trace


instance F.FromField TrailSegment where
  fromField f mbs = case mbs of
    Just bs -> case A.parseOnly parseSegmentRow bs of
      Left err -> error err
      Right v -> return v
    Nothing -> error "foo"

commaP = A.word8 comma

parseSegmentRow :: Parser TrailSegment
parseSegmentRow = parens ts
  where
    ts = TrailSegment <$> fromRowField <*> fromRowField <*> fromRowField <*> fromRowField <*> fromRowField <*> fromRowField <*> fromRowField 

class FromRowField a where
  fromRowField :: Parser a

instance FromRowField Int where
  fromRowField = do
    v <- parseText
    case decimal v of
      Left err -> error err
      Right (v, _) -> return v -- todo figure out remainder
   
instance FromRowField (Maybe T.Text) where
  fromRowField = parseTextM <?> "Text FromRowField"


instance FromRowField Geometry where
  fromRowField = do
    -- consume until: a comma and consume, a rparen and don't consume
    xs <- A.manyTill A.anyWord8 endOfRow
    return . readGeometry . fromStrict . B.pack $ xs

endOfRow = (AC.lookAhead $ A.word8 rparens) <|> commaP


comma, quote, lparens, rparens, lbrace, rbrace :: Word8
comma = 44
quote = 34
lparens = 40
rparens  = 41
lbrace = 123
rbrace = 125


parseText = fmap fromJust parseTextM

parseTextM :: Parser (Maybe T.Text)
parseTextM = do
  xs <- A.manyTill A.anyWord8 endOfRow <?> "manyTill"
  if null xs
    then return Nothing
    else return $ Just . TE.decodeUtf8 . B.pack $ xs


parens :: Parser a -> Parser a
parens p = do
  _ <- word8 lparens
  v <- p
  _ <- word8 rparens
  return v


