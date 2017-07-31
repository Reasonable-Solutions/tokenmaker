module Main where
import           Data.Text          (pack)
import           Prelude            (IO, Maybe (..), putStrLn, show, ($))
import           System.Environment (lookupEnv)
import           Web.JWT            (Algorithm (HS256), def, encodeSigned, iat,
                                     numericDate, secret)

main :: IO ()
main = do
  key <- lookupEnv "key"
  case key of
    Just s -> let claimSet = def { iat = numericDate 12312312 }
      in putStrLn $ show $ encodeSigned HS256 (secret $ pack s) claimSet
    Nothing -> putStrLn "Provide key"
