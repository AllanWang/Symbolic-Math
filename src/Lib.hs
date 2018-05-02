{-# LANGUAGE LambdaCase #-}

module Lib
    ( simplify,
    Op,
    Expr
    ) where

import Data.List (partition)

data Op = Add | Sub | Times | Divide | Pow deriving (Show, Eq)
data Expr = Op Op [Expr] | Int Integer | Const String | Var String | Empty deriving (Show, Eq)

simplify :: Expr -> Expr

-- Basics
simplify Empty = Empty
simplify (Int num) = Int num
simplify (Var "") = Empty
simplify (Var x) = Var x
simplify (Const c) = Const c
simplify (Op _ []) = Empty

simplify (Op _ (h:[])) = simplify h
simplify (Op op l) = clean $ simplify' op l'
  where
  -- Simplify contents of a list and remove empty expressions
  l' = filter (\case Empty -> False; _ -> True) $ map simplify l
  -- Convert empty op to EMPTY, single data to data
  clean = \case [] -> Empty; x:[] -> x; result -> Op op result
  -- Underlying logic for shortening list
  simplifyVal op init l = do
    let (vals, others) = partition (\case Int _ -> True; _ -> False) l
    (Int $ foldr (+) 0 $ map (\(Int x) -> x) vals, others)
  simplify' _ [] = []
  simplify' Add l = uncurry (:) $ simplifyVal (+) 0 l
  simplify' Sub l = uncurry (:) $ simplifyVal (-) 0 l
  simplify' Times l = do
    let (coefficient, others) = simplifyVal (*) 1 l
    if coefficient == Int 0 then [Int 0] else coefficient : others
  simplify' Divide l = uncurry (:) $ simplifyVal (/) 1 l

