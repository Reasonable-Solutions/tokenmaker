{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where
import           Control.Monad      (join)
import           Data.Semigroup     ()
import           Data.Text          (pack)
import           Options.Generic
import           Prelude            (Either (..), IO, Int, Maybe (..),
                                     Show (..), String, putStrLn, show,
                                     undefined, ($), (.), (<$>), (<*>), (>>=))
import           System.Environment (lookupEnv)
import           Web.JWT            (Algorithm (HS256), ClaimsMap, JSON,
                                     NumericDate, Signature, StringOrURI,
                                     VerifiedJWT, aud, decodeAndVerifySignature,
                                     def, encodeSigned, exp, iat, iss, nbf,
                                     numericDate, secret, signature,
                                     stringOrURI, sub)

data Args =
  Create { issuer     :: Maybe String
         , subject    :: Maybe String
         , notBefore  :: Maybe Int
         , expiration :: Maybe Int
         , issuedAt   :: Maybe Int
           --      , miscClaims :: ClaimsMap
         }
  | Validate { token :: String } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = getRecord "jwt gen" >>=
   (\args -> lookupEnv "key" >>=
     \key -> makeJwt key (args :: Args))

validate :: String -> Args -> Maybe Signature
validate key (Validate t) =
  decodeAndVerifySignature (secret $ pack key) (pack t) >>= signature

create :: String -> Args -> JSON
create s (Create is su nb ex at) =
  encodeSigned HS256 (secret $ pack s) claims
  where claims = def { iss = join $ stringOrURI . pack <$> is
                     , sub = join $ stringOrURI . pack <$> su
                     }

makeJwt :: Maybe String -> Args -> IO ()
makeJwt key (Validate t) = case key of
    Just s  -> putStrLn $ show $ validate s (Validate t)
    Nothing -> putStrLn "Provide key"

makeJwt key (Create is su no ex isu) = case key of
  Just s  -> putStrLn $ show $ create s (Create is su no ex isu)
  Nothing -> putStrLn "Provide key"
