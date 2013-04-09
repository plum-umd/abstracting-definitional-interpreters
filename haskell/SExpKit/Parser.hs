{-# LANGUAGE TupleSections #-}

module SExpKit.Parser where

import Control.Monad
import Data.Functor
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import SExpKit.Data
import Text.Parsec
import Text.Parsec.String
import Text.Printf
import qualified Text.Parsec as P

mkSExpQQ :: (ToQQ lit) => (String -> Either ParseError (PreSExpG lit)) -> QuasiQuoter
mkSExpQQ parser = QuasiQuoter expQ patQ undefined undefined
  where
    expQ input = do
      locMsg <- locationError
      case parser input of
        Left e ->
          fail $ "\n" ++ locMsg ++ "\n" ++ show e
        Right s ->
          toQQE s
    patQ input = do
      locMsg <- locationError
      case parser input of
        Left e ->
          fail $ "\n" ++ locMsg ++ "\n" ++ show e
        Right s ->
          toQQP s
    locationError = do
      (Loc fn _ _ (sl, sc) (el, ec)) <- location
      return $ printf "QQ %s[%s:%s-%s:%s]" 
                      fn (show sl) (show sc) (show el) (show ec)

sexp :: QuasiQuoter
sexp = mkSExpQQ (runP (parseSExpTL::Parser (PreSExpG Integer)) () "")

sexpL :: QuasiQuoter
sexpL = mkSExpQQ (runP (parseList::Parser (PreSExpG Integer)) () "")


parseSExpTL :: (Read lit) => Parser (PreSExpG lit)
parseSExpTL = between deadspace deadspace parseSExp

parseSExp :: (Read lit) => Parser (PreSExpG lit)
parseSExp = msum
  [ between (P.char '(') (P.char ')') parseList
  , between (P.char '[') (P.char ']') parseList
  , parseAnti
  , P.try parseLit
  , parseSym
  ]

parseList :: (Read lit) => Parser (PreSExpG lit)
parseList = do
  deadspace
  sexps <- sepEndBy parseSExp deadspace
  case sexps of
    [] -> return PreNull
    _ -> do
      consTail <- parseCons P.<|> return PreNull
      deadspace
      return $ foldr PreCons consTail sexps

parseCons :: (Read lit) => Parser (PreSExpG lit)
parseCons = do
  P.char '.'
  deadspace
  parseSExp

parseAnti :: (Read lit) => Parser (PreSExpG lit)
parseAnti = do
  P.char '@'
  uncurry PreAnti <$> msum
    [ do P.char '$'
         (AntiVal,) <$> antiName
    , do P.try $ P.string "Sym$"
         (AntiSym,) <$> antiName
    , do P.try $ P.string "Lit$"
         (AntiLit,) <$> antiName
    ]
  where
    antiName = msum
     [ P.string "_" >> return Nothing
     , Just <$> tokenSym
     ]
  
parseLit :: (Read lit) => Parser (PreSExpG lit)
parseLit = PreLit <$> tokenLit

tokenLit :: (Read lit) => Parser lit
tokenLit = readParser

parseSym = PreSym <$> tokenSym

tokenSym = P.many1 $ P.noneOf illegal
  where
    -- @ is for anti-patterns (@$x @Sym$x @Lit$x)
    -- # is for nested comments (#| ... |$)
    -- ` is quasiquote
    -- ' is quote
    -- , is unquote
    -- . is cons (a . b)
    -- ; is comment (; ... \n)
    -- " is for strings ("foo")
    illegal = " \t\r\n()[]@#`',.;\""

readParser :: (Read a) => Parser a
readParser = do
  s <- P.getInput
  case reads s of
    [] -> P.parserFail "read failed"
    (x,s'):_ -> do
      P.setInput s'
      return x

deadspace = P.skipMany deadUnit

deadUnit :: Parser ()
deadUnit = whitespace P.<|> singleLineComment P.<|> multilineComment

whitespace :: Parser ()
whitespace = do
  P.oneOf " \t\r\n"
  return ()

singleLineComment :: Parser ()
singleLineComment = do
  P.char ';'
  P.many $ P.noneOf "\r\n"
  P.string "\n" P.<|> P.string "\r\n"
  return ()

multilineComment :: Parser ()
multilineComment = do
  P.string "#|"
  nestedComment 1

nestedComment :: Int -> Parser () 
nestedComment 0 = return ()
nestedComment i = do
  a <- commentAtom
  case a of
    "#|" -> nestedComment (i+1)
    "|#" -> nestedComment (i-1)
    _    -> nestedComment i

commentAtom = msum 
  [ P.try $ P.string "#|"
  , P.try $ P.string "|#"
  , (:[]) <$> P.anyChar
  ]
