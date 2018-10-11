import qualified System.Random as R
import PropLang
import PropLangOps

data Param = Param {ra::Integer, rn::Integer, rc::Integer, rd::Integer, ri::Integer}

randomProp :: R.RandomGen g => Param -> g -> (PropLang, g)
randomProp p g = case R.randomR (0, sum - 1) g of 
    (n, g')
        | n < ra p -> ope0 g' $ Atom "P"
        | n < ra p + rn p -> ope1 g' Not
        | n < ra p + rn p + rc p -> ope2 g' (∧) 
        | n < ra p + rn p + rc p + rd p -> ope2 g' (∨)
        | n < ra p + rn p + rc p + rd p + ri p-> ope2 g' (⊃)
    where sum = ra p + rn p + rc p + rd p + ri p
          ope0 g0 c = (c, g0)
          ope1 g0 c = case randomProp p g0 of
                (f, g') -> ope0 g' $ c f
          ope2 g0 c = case randomProp p g0 of
                (f, g') -> ope1 g' $ c f


randomList :: R.RandomGen g => (g -> (a,g)) -> g -> [a]
randomList r g0 = case r g0 of
    (n, g1) -> n:randomList r g1


