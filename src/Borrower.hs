{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Borrower where

import           Data.Yaml

-- br = Borrower

type Name = String
type MaxBooks = Int

data Borrower = Borrower
  { name     :: Name
  , maxBooks :: MaxBooks
  } deriving (Show, Eq)

instance FromJSON Borrower where
  parseJSON (Object v) = Borrower <$>
                         v .: "name" <*>
                         v .: "max-books"
  parseJSON _ = error "Can't parse Borrower from YAML/JSON"

instance ToJSON Borrower where
  toJSON (Borrower name maxBooks) = object ["name" .= name, "max-books" .= maxBooks]

makeBorrower :: Name -> MaxBooks -> Borrower
makeBorrower = Borrower

getName :: Borrower -> Name
getName Borrower {name} = name

setName :: Name -> Borrower -> Borrower
setName n br@Borrower {name} = br {name = n}

getMaxBooks :: Borrower -> MaxBooks
getMaxBooks Borrower {maxBooks} = maxBooks

setMaxBooks :: MaxBooks -> Borrower -> Borrower
setMaxBooks m br@Borrower {maxBooks} = br {maxBooks = m}

borrowerToString :: Borrower -> String
borrowerToString br = getName br ++ " (" ++ show (getMaxBooks br) ++ " books)"
