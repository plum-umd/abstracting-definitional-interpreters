{-# LANGUAGE TupleSections #-}

module SExpKit.Parser where

import Text.Parsec.String
import Text.Parsec
import Control.Monad
import Data.Functor
import SExpKit.Data
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Printf

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
  [ between (char '(') (char ')') parseList
  , between (char '[') (char ']') parseList
  , parseAnti
  , try parseLit
  , parseSym
  ]

parseList :: (Read lit) => Parser (PreSExpG lit)
parseList = do
  deadspace
  sexps <- sepEndBy parseSExp deadspace
  case sexps of
    [] -> return PreNull
    _ -> do
      consTail <- parseCons <|> return PreNull
      deadspace
      return $ foldr PreCons consTail sexps

parseCons :: (Read lit) => Parser (PreSExpG lit)
parseCons = do
  char '.'
  deadspace
  parseSExp

parseAnti :: (Read lit) => Parser (PreSExpG lit)
parseAnti = do
  char '@'
  uncurry PreAnti <$> msum
    [ do char '$'
         (AntiVal,) <$> antiName
    , do try $ string "Sym$"
         (AntiSym,) <$> antiName
    , do try $ string "Lit$"
         (AntiLit,) <$> antiName
    ]
  where
    antiName = msum
     [ string "_" >> return Nothing
     , Just <$> tokenSym
     ]
  
parseLit :: (Read lit) => Parser (PreSExpG lit)
parseLit = PreLit <$> tokenLit

tokenLit :: (Read lit) => Parser lit
tokenLit = readParser

parseSym = PreSym <$> tokenSym

tokenSym = many1 $ noneOf illegal
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
  s <- getInput
  case readsPrec 0 s of
    [] -> parserFail "read failed"
    (x,s'):_ -> do
      setInput s'
      return x

deadspace = skipMany deadUnit

deadUnit :: Parser ()
deadUnit = whitespace <|> singleLineComment <|> multilineComment

whitespace :: Parser ()
whitespace = do
  oneOf " \t\r\n"
  return ()

singleLineComment :: Parser ()
singleLineComment = do
  char ';'
  many $ noneOf "\r\n"
  string "\n" <|> string "\r\n"
  return ()

multilineComment :: Parser ()
multilineComment = do
  string "#|"
  nestedComment 1
  where
    nestedComment :: Int -> Parser () 
    nestedComment 0 = return ()
    nestedComment i = do
      a <- commentAtom
      case a of
        "#|" -> nestedComment (i+1)
        "|#" -> nestedComment (i-1)
        _    -> nestedComment i

    commentAtom = msum [try (string "#|"), try (string "|#"), (:[]) <$> anyChar]
