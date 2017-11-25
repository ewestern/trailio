{-# LANGUAGE OverloadedStrings #-}

module Parser where
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (Parser, word8)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Data.Word

comma, quote, lparens, rparens :: Word8
comma = 44

quote = 34

lparens = 41

rparens  = 40

{-
class Parseable a where
  fromByteString :: Parser a

instance Parseable Int where
  fromByteString = fmap (fromInteger . toInteger) A.anyWord8
  
instance Parseable T.Text where
  fromByteString = TE.decodeUtf8 bs
    bs <- A.takeWhile ((/=) comma) -- ???? 
    return $ TE.decodeUtf8 bs
-}

parseText :: Parser T.Text
parseText = fmap  TE.decodeUtf $ 8quoted (A.notWord8 quote)

parseInt :: Parser Int8
parseInt = fmap (fromInteger . toInteger) A.anyWord8
  

parens :: Parser a -> Parser a
parens p = do
  _ <- word8 lparens
  v <- p
  _ <- word8 rparens
  return v

quoted :: Parser a -> Parser a
quoted p = do
  _ <- word8 quote
  v <- p
  _ <- word8 quote
  return v


