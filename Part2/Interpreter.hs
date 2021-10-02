module Interpreter where

import AbsNumbers

eval :: Exp -> Integer
eval (Num n) = n
eval (Plus n m) = (eval n) + (eval m)
eval (Times n m) = (eval n) * (eval m)
eval (Minus n m) = (eval n) - (eval m)
eval (Divide n m) = (eval n) `div` (eval m)
eval (Modulus n m) = (eval n) `mod` (eval m)
eval (Exponent n m) = (eval n) ^ (eval m)
eval (Negative n) = - (eval n)
eval (Absolute n) = if (eval n) < 0 
    then (eval n) * (-1)
    else (eval n)