{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Main where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.Char
import Data.List ((\\))

{-
Let us look at parsing a few simple formal languages.
Our goal is to first recognize them and then,
if possible, extract useful information out of them.

Some of them are specified formally in eBNf while
others are just given an informal description.
Those in eBNf share the following common productions.

    space = " " | "\t" | "\n" | "\v" | "\f" | "\r" ;
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    uppercase = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
              | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T"
              | "U" | "V" | "W" | "X" | "Y" | "Z" ;

We shall be working with Megaparsec, a descendant of Parsec.
It is a convenient parser combinator library built on top of
the basic applicative, monadic and alternative type classes.
A simple example of parsing natural numbers follows.
-}

{-
Our first case study is the regular language of Canadian postal codes.

    postalCode = forwardSortationArea , [ space ]
               , localDeliveryUnit ;
    forwardSortationArea = postalDistrict , region , section ;
    localDeliveryUnit = digit , trailingLetter , digit ;
    postalDistrict = initialLetter ;
    region = rural | urban ;
    section = trailingLetter ;
    rural = "0" ;
    urban = digit - rural ;
    trailingLetter = uppercase - ( "D" | "F" | "I" | "O" | "Q" | "U" ) ;
    initialLetter = trailingLetter - "W" | "Z" ;

We would like to convert them into Haskell data structures.

    type PostalCode = ((Char, Int, Char), (Int, Char, Int))
-}

{-
Since we are dealing with parser combinators,
we can also parametrize parsers in terms of other parsers.
Let us do just that by implementing the following context-free language of
keyed recursive listings and extracting the items appropriately.

    listing p = entry p | group p ;
    group p = "[" , listing p , "]" ;
    entry p = identifier , ":" , p ;
    identifier = digit , { digit } ;
-}

{-
Consider a small variation of the listing grammar that
may also contain references to previously identified items.
Such a change makes the language context-sensitive.
-}

{-
We now have the tools and knowledge to
parse all decidable languages that admit a left-recursive parser.
However our journey does not end here.

Practical languages are usually regular or context-free.
Even those categories have two popular subcategories that
deserve special attention.

Flags like options given as arguments to programs or
file permission lists form a permutation language.

    xw
-}

{-
Hierarchies like trees or
simple algebraic expressions form a nested word language.

    -1+2*3-4+5*(-6)+(-7)
-}

{-
It might seem that we can parse all but the most pathological of languages.
Unfortunately some of them are rather tricky to work with.
Try the context-free language of palindromes for example.

    palindrome = "A" | ... | "A" , { palindrome } , "A" | ... ;
-}

main :: IO ()
main = return ()
