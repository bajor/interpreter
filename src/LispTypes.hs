module LispTypes where

data LispVal = 
   LispAtom String
  | LispList [LispVal]
  | LispDottedList [LispVal] LispVal
  | LispNumber Integer
  | LispString String
  | LispBool Bool

-- ADT - Algebraic Data Type - defines set possible values that are possible in Lisp

-- LispDottedList 	- representing the Scheme form (a b . c); also called an improper list. This stores a list of all elements but the last, and then stores the last element as another field

