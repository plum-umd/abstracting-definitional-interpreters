{-# LANGUAGE QuasiQuotes #-}

import SExpKit.Data
import SExpKit.Parser
import Text.Parsec
import qualified Text.PrettyPrint.ANSI.Leijen as PP

foo :: SExp
foo = 
  let [sexp| (foo @Sym$x @$_) |] = [sexp| (foo blah bar) |] 
  in [sexp| 
       (blah blah 
        (foo bar baz) 
        (asldka 3 4 asdfka) 
        (lajasdf a 3 1111 fa sdf asdf (alsdk alskdf) aslkdfj . asdf) 
        (100 asdfa alsdf)) 
     |]

main :: IO ()
main = PP.putDoc $ PP.pretty foo
