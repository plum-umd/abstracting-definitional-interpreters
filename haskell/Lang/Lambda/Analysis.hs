module Lang.Lambda.Analysis where

import Data.Set (Set)
import Lang.Lambda.AST
import qualified Data.Set as Set

children :: Expr -> [Expr]
children (LitE _) = []
children (VarE _) = []
children (LamE _ body) = [body]
children (LetE _ e1 body) = [e1,body]
children (LetRecE _ _ lbody body) = [lbody,body]
children (IfZE c tb fb) = [c,tb,fb]
children (AppE f e) = [f,e]
children (PrimE _ es) = es

subExprs :: Expr -> Set Expr
subExprs e = Set.insert e $ Set.unions $ map subExprs $ children e
