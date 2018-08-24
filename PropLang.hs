module PropLang(PropLang(..), Asmt, match) where

import Data.Map
import Data.String

data PropLang
    = Atom String
    | N PropLang
    | PropLang :∧ PropLang
    | PropLang :∨  PropLang
    | PropLang :⊃ PropLang
    deriving (Eq, Show, Read)
    
infixl 3 :∧
infixl 2 :∨
infixr 1 :⊃

instance IsString PropLang where
  fromString = Atom

type Asmt = Map String PropLang

consistentMerge :: (Ord k, Eq v) => Map k v -> Map k v -> Map k v
consistentMerge a b = if consistent a b then a `union` b else empty
  where consistent c d = and $ elems $ intersectionWith (==) c d

mergeNonEmpty :: (Ord k, Eq a) => Map k a -> Map k a -> Map k a
mergeNonEmpty a b = if a == empty || b == empty then empty
                else consistentMerge a b

match :: PropLang -> PropLang -> Asmt
match pat a = case (pat, a) of
    (Atom s, _ )-> singleton s a
    (N p, N q) -> match p q
    (p1 :∧ p2, q1 :∧ q2) -> mergeNonEmpty (match p1 q1) (match p2 q2)
    (p1 :∨ p2, q1 :∨ q2) -> mergeNonEmpty (match p1 q1) (match p2 q2)
    (p1 :⊃ p2, q1 :⊃ q2) -> mergeNonEmpty (match p1 q1) (match p2 q2)
    _ -> empty