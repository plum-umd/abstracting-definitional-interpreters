module Lang.Lambda.CPS where

import Control.Monad.State
import Lang.Lambda.Data

-- CPS Transformation
-- (uses my[DD] trick for representing meta-continuations to reduce
-- administrative redexes)

fresh :: String -> State Integer String
fresh x = do
  n <- get
  modify (+1)
  return $ x ++ "$" ++ show n

type MetaK = Either Atom (Atom -> State Integer Call)

objectK :: Atom -> MetaK
objectK = Left

reifyMetaK :: MetaK -> State Integer Atom
reifyMetaK (Left a) = return a
reifyMetaK (Right k) = do
  x <- fresh "x"
  let ox = VarA x
  kBody <- k ox
  return $ KonA x kBody

metaK :: (Atom -> State Integer Call) -> MetaK
metaK = Right

callMetaK :: MetaK -> Atom -> State Integer Call
callMetaK (Left ka) a = return $ KAppC ka a
callMetaK (Right k) a = k a

cps :: Expr -> Call
cps e = evalState (cpsMK e $ metaK (return . HaltC)) 0

cpsFresh :: Expr -> State Integer (Call,String)
cpsFresh e = do
  k <- fresh "k"
  call <- cpsMK e $ objectK $ VarA k
  return (call,k)

cpsMK :: Expr -> MetaK -> State Integer Call
cpsMK (LitE n) mk = callMetaK mk $ LitA n
cpsMK (VarE x) mk = callMetaK mk $ VarA x
cpsMK (LamE x body) mk = do
  (bodyCall,k) <- cpsFresh body
  callMetaK mk $ LamA x k bodyCall
cpsMK (LetE x e1 e2) mk =
  cpsMK e1 $ metaK $ \ e1a -> do
    e2Call <- cpsMK e2 mk
    return $ LetC x e1a e2Call  
cpsMK (LetRecE f x body e) mk = do
  (bodyCall,k) <- cpsFresh body
  cpsMK e $ metaK $ \ ea -> do
    eCall <- callMetaK mk ea
    return $ LetRecC f x k bodyCall eCall
cpsMK (IfZE c tb fb) mk =
  cpsMK c $ metaK $ \ ca -> do
    ok <- reifyMetaK mk
    k <- fresh "k"
    tbCall <- cpsMK tb $ objectK $ VarA k
    fbCall <- cpsMK fb $ objectK $ VarA k
    return $ LetC k ok $ IfZC ca tbCall fbCall
cpsMK (AppE f e) mk =
  cpsMK f $ metaK $ \ fa ->
    cpsMK e $ metaK $ \ ea -> do
      ok <- reifyMetaK mk
      return $ LAppC fa ea ok
cpsMK (PrimE op es) mk =
  mapCPS es $ \ eas ->
    callMetaK mk $ PrimA op eas

mapCPS :: [Expr] -> ([Atom] -> State Integer Call) -> State Integer Call
mapCPS [] k = k []
mapCPS (e:es) k =
  cpsMK e $ metaK $ \ a ->
    mapCPS es $ \ as -> k (a:as)
