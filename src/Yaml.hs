{-# LANGUAGE OverloadedStrings #-}


module Yaml where

--import Data.Yaml
--import Control.Applicative -- <$>, <*>
--import Data.Maybe
--import GHC.Generics

import           Book
import           Borrower

--import qualified Data.ByteString.Char8 as BS

--instance FromJSON Borrower where
--  parseJSON (Object v) = Borrower <$>
--                         v .: "name" <*>
--                         v .: "max-books"
--  -- A non-Object value is of the wrong type, so fail.
--  parseJSON _ = error "Can't parse Borrower from YAML/JSON"

--instance FromJSON Book where
--    parseJSON (Object v) = Book <$>
--                           v .: "title" <*>
--                           v .: "author" <*>
--                           v .:? "borrower"
--    -- A non-Object value is of the wrong type, so fail.
--    parseJSON _ = error "Can't parse Book from YAML/JSON"

--instance FromJSON Borrower
--instance FromJSON Book

--mainly = do
--         ymlData <- BS.readFile "borrowers-before.yml"
--         ymlData2 <- BS.readFile "books-before.yml"
--         let borrowers = Data.Yaml.decode ymlData :: Maybe [Borrower]
--             books = Data.Yaml.decode ymlData2 :: Maybe [Book]
--         -- Print it, just for show
--         print $ fromJust borrowers
--         print $ fromJust books
