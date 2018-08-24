import Data.List
import PropLang
import PropLangOps
import Hilbert

infers :: [PropLang] -> [Maybe InferDetail]
infers a = takeWhile ( /= Nothing) $ map inferlast $ drop 1 $ inits a
  where inferlast = do
                      y <- init
                      z <- last
                      return $ infer y z
                      
getTheorem :: [PropLang] -> PropLang
getTheorem s = fst $ last $ zip s (infers s)
  
ex1 :: [PropLang]
ex1 = [
    (p ⊃ (p ⊃ p) ⊃ p  ) ⊃ (p ⊃ p ⊃ p) ⊃ (p ⊃ p),
    p ⊃ (p ⊃ p) ⊃ p,
    (p ⊃ p ⊃ p) ⊃ (p ⊃ p),
    p ⊃ p ⊃ p,
    p ⊃ p
    ]

ex2 :: [PropLang]
ex2 = [
    q ⊃ p ⊃ q∧p,
    (q ⊃ p ⊃ q∧p) ⊃ p∧q ⊃ q ⊃ p ⊃ q∧p,
    p∧q ⊃ q ⊃ p ⊃ q∧p,
    (p∧q ⊃ q ⊃ p ⊃ q∧p) ⊃ (p∧q ⊃ q) ⊃ p∧q ⊃ p ⊃ q∧p,
    p∧q ⊃ q,
    (p∧q ⊃ q) ⊃ p∧q ⊃ p ⊃ q∧p,
    p∧q ⊃ p ⊃ q∧p,
    (p∧q ⊃ p ⊃ q∧p) ⊃ (p∧q ⊃ p) ⊃ p∧q ⊃ q∧p,
    p∧q ⊃ p,
    (p∧q ⊃ p) ⊃ p∧q ⊃ q∧p,
    p∧q ⊃ q∧p
    ]

main :: IO ()
main = do
--    print $ infers ex1
--    putStrLn $ intercalate "\n" $ map show $ infers ex2
    putStr $ show $getTheorem ex1
    putStrLn " is a theorem"
    putStr $ show $getTheorem ex2
    putStrLn " is a theorem"
