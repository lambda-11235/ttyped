
module Tests.Util (typeChecks, isInNormalForm) where

import Check
import Reduce
import Representation


typeChecks :: Term -> Bool
typeChecks t = case checkTerm t Star of
                 Left _ -> False
                 Right () -> True


isInNormalForm :: Term -> Bool
isInNormalForm (C ctx) = nfCtx ctx
isInNormalForm (O obj) = nfObj obj

nfCtx Star = True
nfCtx (Quant _ term ctx) = isInNormalForm term && nfCtx ctx

nfObj (Var _ _) = True
nfObj (Prod _ term obj) = isInNormalForm term && nfObj obj
nfObj (Fun _ term obj) = isInNormalForm term && nfObj obj
nfObj (App (Var _ _) obj) = nfObj obj
nfObj (App _ _) = False
