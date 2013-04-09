module Lang.Lambda.Analysis where

import Data.Set (Set)
import Lang.Lambda.Data
import qualified Data.Set as Set

subExprs :: Expr -> Set Expr
subExprs e =
  Set.insert e $ Set.unions $ map subExprs $ case e of
    LitE _ -> []
    VarE _ -> []
    LamE _ body -> [body]
    LetE _ e1 body -> [e1,body]
    LetRecE _ _ lbody body -> [lbody,body]
    IfZE c tb fb -> [c,tb,fb]
    AppE f e -> [f,e]
    PrimE _ es -> es
