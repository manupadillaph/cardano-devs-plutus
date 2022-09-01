{-# LANGUAGE OverloadedStrings          #-}

module Main where

--import qualified Data.Text                           as DataText (pack, unpack, Text) --, 
import Data.ByteString.Char8 as C8
import Data.ByteString as BS

import           System.Environment       (getArgs)

--import qualified Utils                    (myConsByteString)

--Modulo: 

main :: IO ()
main = do
    [str,n] <- getArgs
    let 
        res = BS.cons (fromIntegral (read n)) (C8.pack str)
    Prelude.putStrLn (C8.unpack res)
