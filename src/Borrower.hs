{-# LANGUAGE NamedFieldPuns, DeriveGeneric #-}

module Borrower where

import GHC.Generics

-- br = Borrower

type Name = String
type MaxBooks = Int

data Borrower = Borrower
  { name     :: Name
  , maxBooks :: MaxBooks
  } deriving (Show, Eq, Generic)

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
