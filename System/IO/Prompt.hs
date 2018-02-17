{-

console-prompt
Copyright (C) 2017, 2018 Jonathan Lamothe
<jlamothe1980@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program.  If not, see
<http://www.gnu.org/licenses/>.

-}

module System.IO.Prompt where

import Data.Functor
import System.IO

-- | Prompts the user for a string
prompt
  :: String
  -- ^ The prompt to display
  -> IO String
prompt pStr = do
  putStr pStr
  hFlush stdout
  getLine

-- | Prompts the user and converts the value to a specific type
promptWith
  :: (String -> a)
  -- ^ The conversion function
  -> String
  -- ^ The prompt to display
  -> IO a
promptWith f pStr = f <$> prompt pStr

-- | Prompts the user and converts the value to a specific type with a
-- possibility of failing with a single error message, repeating until
-- a valid entry is given
promptWithMaybe
  :: (String -> Maybe a)
  -- ^ The conversion function (returns nothing on failure)
  -> String
  -- ^ The prompt to display
  -> String
  -- ^ The error message to display on failure
  -> IO a
promptWithMaybe f pStr errMsg = do
  x <- promptWith f pStr
  case x of
    Nothing -> do
      putStrLn errMsg
      promptWithMaybe f pStr errMsg
    Just x -> return x

-- | Prompts the user and converts the value to a specific type with a
-- possibility of failing with multiple error messages, repeating
-- until a valid entry is given
promptWithEither
  :: (String -> Either String a)
  -- ^ The conversion function (returns the error message on failure)
  -> String
  -- ^ The prompt to display
  -> IO a
promptWithEither f pStr = do
  x <- promptWith f pStr
  case x of
    Left msg -> do
      putStrLn msg
      promptWithEither f pStr
    Right x -> return x

-- | Prmopts the user for a single character
charPrompt
  :: String
  -- ^ The prompt to display
  -> IO Char
charPrompt pStr = do
  bufMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  putStr pStr
  x <- getChar
  putStrLn ""
  hSetBuffering stdin bufMode
  return x

-- | Prompts the user for a character and converts the value to a
-- specific type
charPromptWith
  :: (Char -> a)
  -- ^ The conversion function
  -> String
  -- ^ The prompt to display
  -> IO a
charPromptWith f pStr = f <$> charPrompt pStr

-- | Prompts the user for a character and converts the value to a
-- specific type with a possibility of failing with a single error
-- message, repeating until a valid entry is given
charPromptWithMaybe
  :: (Char -> Maybe a)
  -- ^ The conversion function (returns nothing on failure)
  -> String
  -- ^ The prompt to display
  -> String
  -- ^ The error message to display on failure
  -> IO a
charPromptWithMaybe f pStr eMsg = do
  x <- charPromptWith f pStr
  case x of
    Nothing -> do
      putStrLn eMsg
      charPromptWithMaybe f pStr eMsg
    Just x -> return x

-- | Prompts the user for a character and converts the value to a
-- specific type with a possibility of failing with multiple error
-- messages, repeating until a valid entry is given
charPromptWithEither
  :: (Char -> Either String a)
  -- ^ The conversion function (returns the error message on failure)
  -> String
  -- ^ The prompt to display
  -> IO a
charPromptWithEither f pStr = do
  x <- charPromptWith f pStr
  case x of
    Left msg -> do
      putStrLn msg
      charPromptWithEither f pStr
    Right x -> return x

-- jl
