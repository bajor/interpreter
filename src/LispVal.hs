module LispVal (LispVal) where

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

-- ADT - Algebraic Data Type - defines set possible values that are possible in Lisp

-- Atom 	- stores a String naming the atom
-- List 	- stores a list of other LispVals; also called a proper list
-- DottedList 	- representing the Scheme form (a b . c); also called an improper list. This stores a list of all elements but the last, and then stores the last element as another field
-- Number 	- Haskell Integer
-- String 	- Haskell String
-- Bool 	- Haskell boolean value

-- Constructors and types have different namespaces, so you can have both a constructor named String and a type named String. Both types and constructor tags always begin with capital letters. 

