{-# LANGUAGE NamedFieldPuns #-}

module Brook where

type Name = String
type MaxBooks = Int
type Title = String
type Author = String

data Brook = Br
            { name     :: Name
            , maxBooks :: MaxBooks
            }
          | Bk
            { title    :: Title
            , author   :: Author
            , borrower :: Maybe Brook
            } deriving (Show, Eq)

type Brooks = ([Brook], Bool)

makeBr :: Name -> MaxBooks -> Brook
makeBr = Br

makeBk :: Title -> Author -> Maybe Brook -> Brook
makeBk = Bk

getBrName :: Brook -> Maybe Name
getBrName Br {name} = Just name
getBrName _ = Nothing

addBrk :: Brook -> Brooks -> Brooks
addBrk brk brksb =
  if null coll
    then (brks ++ [brk], True)
    else (brks, False)
      where brks = fst brksb
            coll = filter (== brk) brks

borrower1 = Br {name = "Borrower1", maxBooks = 1}
borrower2 = Br {name = "Borrower2", maxBooks = 2}
borrower3 = Br {name = "Borrower3", maxBooks = 3}

borrowers1 = ([borrower1, borrower2], True)
borrowers2 = ([borrower1, borrower2, borrower3], True)
borrowers3 = ([borrower1, borrower2, borrower3], False)

book1 = Bk { title = "Title1"
           , author = "Author1"
           , borrower = Just borrower1 }
book2 = Bk { title = "Title2"
           , author = "Author2"
           , borrower = Nothing }
book3 = Bk { title = "Title3"
           , author = "Author3"
           , borrower = Just borrower3 }
book4 = Bk { title = "Title4"
           , author = "Author4"
           , borrower = Just borrower3 }

books1 = ([book1, book2], True)
books2 = ([book1, book2, book3], True)
books3 = ([book1, book2, book3, book4], True)
books4 = ([book1, book2, book3, book4], False)
books5 = ([book1, book2, book3], False)
