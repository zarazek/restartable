module Main where

import Control.Monad.Restartable
import System.IO
import Test.RandomStrings

generatePassword :: Restartable String
generatePassword = step "generatePassword" $ randomString (onlyAlphaNum randomASCII) 8

showPassword :: String -> Restartable ()
showPassword pwd = step "showPassword" $ putStrLn $ "The password is " <> pwd

askForPassword :: Int -> Int -> Restartable String
askForPassword usedN allN = step "askForPassword" $ putStr msg >> getLine
  where
    msg = "Please repeat the password (try " <> usedStr <> " of " <> allStr <> "): "
    usedStr = show (usedN + 1)
    allStr = show allN

passwordAccepted :: Restartable ()
passwordAccepted = step "passwordAccepted" $ putStrLn "Password accepted"

passwordRejected :: Restartable ()
passwordRejected = step "passwordRejected" $ putStrLn "Password rejected"

allTriesExhausted :: Restartable ()
allTriesExhausted = step "allTriesExhausted" $ putStrLn "All tries exhausted"

checkPassword :: Int -> String -> Restartable Bool
checkPassword triesAllowed expected = go triesAllowed
  where
    go triesLeft
      | triesLeft <= 0 = do
          allTriesExhausted
          pure False
      | otherwise = do
          actual <- askForPassword (triesAllowed - triesLeft) triesAllowed
          if actual == expected then do
            passwordAccepted
            pure True
          else do
            passwordRejected
            go (triesLeft - 1)

workflow :: Int -> Restartable Bool
workflow numOfTries = do
  pwd <- generatePassword
  showPassword pwd
  checkPassword numOfTries pwd

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  runRestartable "save.txt" (workflow 4) >>= print
