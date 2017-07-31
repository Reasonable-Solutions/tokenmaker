{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where
import           Data.Semigroup     ()
import           Data.Text          (pack)
import           Options.Generic
import           Prelude            (Either (..), IO, Int, Maybe (..),
                                     Show (..), String, putStrLn, show, ($))
import           System.Environment (lookupEnv)
import           Web.JWT            (Algorithm (HS256), ClaimsMap, NumericDate,
                                     StringOrURI, aud, def, encodeSigned, exp,
                                     iat, iss, nbf, numericDate, secret, sub)

data Args = Args { issuer     :: Maybe String <?> "Optional, the issuer"
                 , subject    :: Maybe String <?> "Optional, the subject"
                 , notBefore  :: Maybe Int <?> "Optional valid from"
                 , expiration :: Maybe Int <?> "Optional expiration date"
                 , issuedAt   :: Maybe Int <?> "Optional, issued at"
           --      , miscClaims :: ClaimsMap
                 } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
  args <- getRecord "jwt generator"
  key <- lookupEnv "key"
  makeJwt key (args :: Args)



makeJwt :: Maybe String -> Args -> IO ()
makeJwt key _ =   case key of
    Just s -> let claimSet = def { exp = numericDate 3
                                 }
      in putStrLn $ show $ encodeSigned HS256 (secret $ pack s) claimSet
    Nothing -> putStrLn "Provide key"
