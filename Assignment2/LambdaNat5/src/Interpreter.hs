module Interpreter where

import AbsLambdaNat
import ErrM
import PrintLambdaNat

execCBN :: Program -> [Exp]
execCBN (Prog []) = []
execCBN (Prog (e:es)) = (evalCBN e):(execCBN (Prog es))

evalCBN :: Exp -> Exp
evalCBN (EApp e1 e2) = case (evalCBN e1) of
    (EAbs i e3) -> evalCBN (subst i e2 e3)
    e3 -> EApp e3 e2
evalCBN (EIf e1 e2 e3 e4) = if (evalCBN e1) == (evalCBN e2) then evalCBN e3 else evalCBN e4
evalCBN (ELet i e1 e2) = evalCBN (EApp (EAbs i e2) e1) 
evalCBN (ERec i e1 e2) = evalCBN (EApp (EAbs i e2) (EFix (EAbs i e1)))
evalCBN (EFix e) = evalCBN (EApp e (EFix e)) 
evalCBN (ENil) = ENil
evalCBN (ECons e1 e2)  = ECons (evalCBN e1) (evalCBN e2)
evalCBN (EHd e) = case (evalCBN e) of
    ENil -> ENil
    (ECons e1 e2) -> evalCBN e1
evalCBN (ETl e)  = case (evalCBN e) of
    ENil -> ENil
    (ECons e1 e2) -> evalCBN e2
evalCBN (ELE e1 e2) = case (evalCBN e1) of
    (EInt n) -> case (evalCBN e2) of
        (EInt m) -> if n <= m then (EInt 1) else (EInt 0)

evalCBN (EPlus e1 e2) = case (evalCBN e1) of
    (EInt n) -> case (evalCBN e2) of
        (EInt m) -> EInt (n+m)
        e2' -> EPlus (EInt n) e2'
    e1' -> case (evalCBN e2) of 
        (EInt m) -> EPlus e1' (EInt m)
        e2' -> EPlus e1' e2'
evalCBN (EMinus e1 e2) = case (evalCBN e1) of
    (EInt n) -> case (evalCBN e2) of
        (EInt m) -> EInt (n-m)
        e2' -> EMinus (EInt n) e2'
    e1' -> case (evalCBN e2) of 
        (EInt m) -> EMinus e1' (EInt m)
        e2' -> EMinus e1' e2'
evalCBN (ETimes e1 e2) = case (evalCBN e1) of
    (EInt n) -> case (evalCBN e2) of
        (EInt m) -> EInt (n*m)
        e2' -> ETimes (EInt n) e2'
    e1' -> case (evalCBN e2) of 
        (EInt m) -> ETimes e1' (EInt m)
        e2' -> ETimes e1' e2'
evalCBN (EInt n) = EInt n
evalCBN x = x 
---------------------------------- ^^^^^^^^^^^^^^^^^^^^^^^^^^^ (END HERE)

newtype IDM m a = IDM{unIDM :: m}


efoldMap :: (Data a, Monoid m) => (a -> m) -> a -> m
efoldMap f x = traverse f x
    where
        traverse :: (Data a, Data b, Monoid m) => (a -> m) -> b -> m
        traverse f x = unIDM $ gfoldl (k f) z x
        z :: Monoid m => g -> IDM m g
        z _ = IDM mempty
        k :: Data d => (a -> m) -> IDM m (d -> b) -> d -> IDM m b
        k f (IDM m) d = case cast d of
            Just a  -> IDM (m <> f a)
            Nothing -> IDM m

-- a quick and dirty way of getting fresh names, rather inefficient for big terms...
fresh_aux :: Exp -> String
fresh_aux (EVar (Id i)) = i ++ "0"
fresh_aux (EApp e1 e2) = fresh_aux e1 ++ fresh_aux e2
fresh_aux (EAbs (Id i) e) = i ++ fresh_aux e
fresh_aux _ = "0"

fresh = Id . (pickFresh $ infList) . S.fromList . fresh_aux
  where
    pickFresh :: [String] -> S.Set String -> String
    pickFresh (x:xs) ys | x `S.member` ys = pickFresh xs ys
                        | otherwise = x
    infList = map (:[]) ['a'..'z'] ++ infList_ 1
    infList_ n = map (\a -> a : show n) ['a'..'z'] ++ infList_ (n+1)

newtype ID a = ID{unID :: a}

emap :: (Data a) => (a -> a) -> a -> a
emap f x = traverse x
    where
        traverse :: Data a => a -> a
        traverse x = unID $ gfoldl k z x
        z = ID
        k (ID ca) b  = case castfn f of
            Just fb -> ID (ca (fb b))
            Nothing -> ID (ca b)

        castfn :: (Typeable a, Typeable b, Typeable c, Typeable d) =>
               (a -> b) -> Maybe (c -> d)
        castfn f = cast f

-- beta reduction (capture avoiding substitution)
subst :: Id -> Exp -> Exp -> Exp
subst id s (EVar id') | id == id' = s
                      | otherwise = EVar id'
subst id s (EApp e1 e2) = EApp (subst id s e1) (subst id s e2)
subst id s e@(EAbs id' e') = 
    -- We substiture ID w/ a fresh name in the body of λ abstraction to avoid variable capture 

    let f = fresh e 
        e'' = subst id' (EVar f) e' in 
        EAbs f $ subst id s e''
subst id s e = emap (subst id s) e -- CHECK/TEST THIS LINE JESSE
---
-- subst id s (EIf e1 e2 e3 e4) = EIf (subst id s e1) (subst id s e2) (subst id s e3) (subst id s e4)
-- subst id s (ELet i e1 e2) = subst id s (EApp (EAbs i e2) e1)
-- subst id s (ERec i e1 e2) = subst id s (EApp (EAbs i e2) (EFix (EAbs i e1)))
-- subst id s (EFix e) = EFix (subst id s e)
-- subst id s (EInt n) = EInt n
-- subst id s (EPlus e l) = EPlus (subst id s e) (subst id s l)
-- subst id s (EMinus e l) = EMinus (subst id s e) (subst id s l)
-- subst id s (ETimes e l) = ETimes (subst id s e) (subst id s l)
-- add the missing cases

