{-# LANGUAGE NamedFieldPuns #-}

module Book where

import Person

type Title = String
type Author = String

data Book = Book { title :: Title
                 , author :: Author
                 , borrower :: Maybe Person
                 } deriving (Show, Eq)

bk = Book {title = "Title", author = "Author", borrower = Person {name = "Three", maxBooks = 3}}
bks = [Book {title = "Title2", author = "Author2", borrower = Person {name = "ThreeX", maxBooks = 33}}
      ,Book {title = "Title3", author = "Author3", borrower = Person {name = "ThreeY", maxBooks = 39}}]

makeBook :: Title -> Author -> Person -> Book
makeBook = Book

getTitle :: Book -> Title
getTitle Book {title} = title

-- setTitle :: Title -> Book -> Book
-- setTitle t b@Book {title} = b {title = t}
--
-- getAuthor :: Book -> Author
-- getAuthor Book {author} = author
--
-- setAuthor :: Author -> Book -> Book
-- setAuthor a b@Book {author} = b {author = a}
--
-- getBorrower :: Book -> Person
-- getBorrower Book {borrower} = borrower


--     public void setPerson(Person p2) {
--         this.person = p2;
--     }
--
--     public Person getPerson() {
--         return this.person;
--     }
--
--     public String toString() {
--         String available;
--
--         if (this.getPerson() == null) {
--             available = "Available";
--         } else {
--             available = "Checked out to " +
--                     this.getPerson().getName();
--         }
--
--         return this.getTitle() + " by " + this.getAuthor() +
--                 "; " + available;
--     }
-- }
