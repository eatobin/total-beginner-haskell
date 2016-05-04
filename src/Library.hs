{-# LANGUAGE NamedFieldPuns #-}

module Library where

-- br = Borrower
-- brs = [br]
-- brsb = (brs, Bool)
-- bk = Book
-- bks = [bk]
-- bksb = (bks, Bool)

import           Borrower
import           Book
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe

type Borrowers = ([Borrower], Bool)
type Books = ([Book], Bool)

--data Library = Library { libName :: LibraryName
--                       , libBorrowers :: Borrowers
--                       , libBooks :: Books } deriving (Show, Eq)

--makeLibrary :: LibraryName -> Borrowers -> Books -> Library
--makeLibrary = Library

--getLibName :: Library -> LibraryName
--getLibName Library {libName} = libName

--getLibBorrowers :: Library -> Borrowers
--getLibBorrowers Library {libBorrowers} = libBorrowers

--getLibBooks :: Library -> Books
--getLibBooks Library {libBooks} = libBooks

addBorrower :: Borrower -> Borrowers -> Borrowers
addBorrower br brsb = if null coll then (brs ++ [br], True) else (brs, False)
  where brs = fst brsb
        coll = filter (\cbr -> cbr == br) brs

--addBorrower :: Borrower -> Library -> Library
--addBorrower br l = l {libBorrowers = nbrs}
--  where nbrs = (getLibBorrowers l) ++ [br]

--findBorrower :: Name -> Borrowers -> Borrower
--findBorrower n brs = head [ br | br <- brs, getName br == n ]
---- getBorrower n brs = head $ filter (\br -> getName br == n) brs

addBook :: Book -> Books -> Books
addBook bk bksb = if null coll then (bks ++ [bk], True) else (bks, False)
  where bks = fst bksb
        coll = filter (\cbk -> cbk == bk) bks

removeBook :: Book -> Books -> Books
removeBook tbk bksb = if not (null coll) then ([ bk | bk <- bks, bk /= tbk], True) else (bks, False)
  where bks = fst bksb
        coll = filter (\cbk -> cbk == tbk) bks

--addBook :: Book -> Library -> Library
--addBook b l = l {libBooks = nbks}
--  where nbks = (getLibBooks l) ++ [b]

-- findBook :: Title -> Books -> Book
-- findBook t bs = head [ b | b <- bs, getTitle b == t ]

findBook :: Title -> Books -> Maybe Book
findBook t bksb = if null coll then Nothing else Just (head coll)
  where coll = [ bk | bk <- bks, getTitle bk == t ]
        bks = fst bksb

findBorrower :: Name -> Borrowers -> Maybe Borrower
findBorrower n brsb = if null coll then Nothing else Just (head coll)
  where coll = [ br | br <- brs, getName br == n ]
        brs = fst brsb

getBooksForBorrower :: Borrower -> Books -> [Book]
getBooksForBorrower br bksb = [bk | bk <- bks, getBorrower bk == Just br]
  where bks = fst bksb

--getBooksForBorrower :: Borrower -> Library -> Books
--getBooksForBorrower br l = [b | b <- bs, getBorrower b == (Just br)]
--  where bs = (getLibBooks l)

--setLibBorrower :: Maybe Borrower -> Book -> Library -> Library
--setLibBorrower mp b l =
--  where nb = (setBorrower mp b)

-- checkOut :: Book -> Borrower -> Books -> Books
-- checkOut b br bs =
--   if notMaxedOut && bookNotOut
--     then addBook newBook fewerBooks
--     else bs
--       where booksOut = length (getBooksForBorrower br bs)
--             maxBooksAllowed = getMaxBooks br
--             notMaxedOut = booksOut < maxBooksAllowed
--             bookNotOut = isNothing (getBorrower b)
--             newBook = setBorrower (Just br) b
--             fewerBooks = removeBook b bs

checkOut :: Name -> Title -> Borrowers -> Books -> Books
checkOut n t brsb bksb =
  if notMaxedOut && bookNotOut
    then addBook newBook fewerBooks
    else (bks, False)
      where bks = fst bksb
            bk = fromJust (findBook t bksb)
            br = fromJust (findBorrower n brsb)
            booksOut = length (getBooksForBorrower br bksb)
            maxBooksAllowed = getMaxBooks br
            notMaxedOut = booksOut < maxBooksAllowed
            bookNotOut = isNothing (getBorrower bk)
            newBook = setBorrower (Just br) bk
            fewerBooks = removeBook bk bksb

checkIn :: Title -> Books -> Books
checkIn t bksb =
  if bookOut
    then addBook newBook fewerBooks
    else (bks, False)
      where bks = fst bksb
            bk = fromJust (findBook t bksb)
            bookOut = isJust (getBorrower bk)
            newBook = setBorrower Nothing bk
            fewerBooks = removeBook bk bksb

libraryToString :: Books -> Borrowers -> String
libraryToString bksb brsb = "Test Library: " ++
  show (length bks) ++ " books; " ++
  show (length brs) ++ " people."
    where bks = fst bksb
          brs = fst brsb

statusToString :: Books -> Borrowers -> String
statusToString bksb brsb = "\n" ++
  "--- Status Report of Test Library ---\n" ++
  "\n" ++
  libraryToString bksb brsb ++
  "\n" ++
  unlines (map bookToString bks) ++ "\n" ++
  unlines (map borrowerToString brs) ++ "\n" ++
  "--- End of Status Report ---" ++
  "\n"
    where bks = fst bksb
          brs = fst brsb
