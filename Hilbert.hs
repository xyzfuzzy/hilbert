module Hilbert where

import Data.Map(empty)
import Data.Maybe
import PropLang(PropLang(N, (:⊃)), Asmt, match)
import PropLangOps

axiomtemplate :: [PropLang]
axiomtemplate = [
    p ⊃ q ⊃ p,
    (p ⊃ q ⊃ r) ⊃ (p ⊃ q) ⊃ (p ⊃ r),
    p ⊃ q ⊃ p ∧ q,
    p ∧ q ⊃ p,
    p ∧ q ⊃ q,
    p ⊃ p ∨ q,
    q ⊃ p ∨ q,
    (p ⊃ r) ⊃ (q ⊃ r) ⊃ p ∨ q ⊃ r,
    (N p ⊃ N q) ⊃ q ⊃ p
    ]

ifaxiom :: PropLang -> Maybe PropLang
ifaxiom a = listToMaybe [ x | x <- axiomtemplate, match x a /= empty ]

mpable :: [PropLang] -> PropLang -> Maybe PropLang
mpable a b = listToMaybe [ y | x <- a, y <- a, mp x y == Just b ]

mp :: PropLang -> PropLang -> Maybe PropLang
mp a ab = case ab of
    a2 :⊃ b | a == a2 -> Just b
    _ -> Nothing

data InferDetail
    = Axiom PropLang Asmt
    | MP PropLang
    deriving (Show, Eq)

infer :: [PropLang] -> PropLang -> Maybe InferDetail
infer ctx a = case (ifaxiom a, mpable ctx a) of
  (Just ax, _) -> Just $ Axiom ax (match ax a)
  (_, Just ab) -> Just $ MP ab
  _ -> Nothing