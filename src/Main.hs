{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import           Data.Map
import           Data.Semigroup     ()
import           Data.Text          (pack)
import           Options.Generic
import           Prelude            (Either (..), IO, Maybe (..), Show (..),
                                     putStrLn, show, ($))
import           System.Environment (lookupEnv)
import           Web.JWT            (Algorithm (HS256), ClaimsMap,
                                     NumericDate (..), StringOrURI (..), aud,
                                     def, encodeSigned, exp, iat, iss, nbf,
                                     numericDate, secret, sub)

data Args = Args { issuer     :: Maybe StringOrURI
                 , subject    :: Maybe StringOrURI
                 , notBefore  :: Maybe NumericDate
                 , expiration :: Maybe NumericDate
                 , issuedAt   :: Maybe NumericDate
           --      , miscClaims :: ClaimsMap
                 } deriving (Show, Generic)



main :: IO ()
main = do
  claims <- getRecord "Jwt Token Maker"
  key <- lookupEnv "key"
  case key of
    Just s -> let claimSet = def { exp = numericDate 3
                                 }
      in putStrLn $ show $ encodeSigned HS256 (secret $ pack s) claimSet
    Nothing -> putStrLn "Provide key"
