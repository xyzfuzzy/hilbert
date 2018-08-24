{-# LANGUAGE OverloadedStrings #-} 

module PropLangOps((∧), (∨), (⊃), p, q, r) where

import PropLang

(∧) = (:∧)
(∨) = (:∨)
(⊃) = (:⊃)

infixl 3 ∧
infixl 2 ∨
infixr 1 ⊃

p :: PropLang
p = "p"

q :: PropLang
q = "q"

r :: PropLang
r = "r"