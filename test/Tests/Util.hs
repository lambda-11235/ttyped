
module Tests.Util (nfTerm, nfContext, nfObject) where

import Check
import Reduce
import Representation


nfTerm :: Term -> Bool
nfTerm (C ctx) = nfContext ctx
nfTerm (O obj) = nfObject obj

nfContext :: Context -> Bool
nfContext Star = True
nfContext (Quant _ term ctx) = nfTerm term && nfContext ctx

nfObject :: Object -> Bool
nfObject (App (Fun _ _ _) _) = False
nfObject (Var _ _) = True
nfObject (Prod _ term obj) = nfTerm term && nfObject obj
nfObject (Fun _ term obj) = nfTerm term && nfObject obj
nfObject (App obj1 obj2) = nfObject obj1 && nfObject obj2
