{- Assignment 3
 - Name: Aarsh Patel
 - Date: Oct 25th, 2018
 -}
module Assign_3 where
import Test.QuickCheck
macid :: String
macid = "patea80"


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - polyValue evaluates the polynomial at a given value n
 - This function recursively evaluates for n
 - Input: A polynomial (Poly a), and evaluation number (a) 
 - Output: Returns number evaluated at polynomial (a)
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue X n          = n                                     --Base case, define evaluation of X, which is just f(n) = n
polyValue (Coef p) n   = p                                     --Base case, define evaluation of constant, which is just constant
polyValue (Sum x y) n  = polyValue (x) (n) + polyValue (y) (n) --Defines evaluation of sum of monomials, which is the sum of all the terms evaluated at n
polyValue (Prod x y) n = polyValue (x) (n) * polyValue (y) (n) --Defines evaluation of product of monomials, which is product of all the terms evaluated at n

{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Given a monomial, or polynomial, this function will return the number of highest degree
 - This function determines the highest degree by recursively evaluating the polynomial/monomial
 - Input: A polynomial (Poly a)
 - Output: Highest degree exponent (Integer)
 -}
polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree X          = 1                                                                   --Base case, defines order of X, which is 1
polyDegree (Coef a)   = 0                                                                   --Base case, defines order of constant, which is 0
polyDegree (Prod x y) = polyDegree x + polyDegree y                                         --Defines sum of exponents for products of polynomials
polyDegree (Sum x y)  = if(polyDegree x > polyDegree y) then polyDegree x else polyDegree y --Defines largest order/exponent for a sum of polynomials

{- -----------------------------------------------------------------
 - polyDeriv
 - -----------------------------------------------------------------
 - Given a monomial or polynomial, this function will return the derivative
 - This function recursively determines the derivative using laws of calculus
 - Input: A polynomial (Poly a)
 - Output: Derivative of polynomial (Poly a)
 -}
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv X          = Coef 1                                            --Base case, defines derivative of just X, which is 1 
polyDeriv (Coef a)   = Coef 0                                            --Base case, defines derivative of constant, which is 0
polyDeriv (Sum x y)  = Sum (polyDeriv x) (polyDeriv y)                   --Defines sumation rule of derivatives (x+y)' = x' + y'
polyDeriv (Prod x y) = Sum (Prod x (polyDeriv y)) (Prod y (polyDeriv x)) --Defines product rule of derivatives  (x*y)' = x*y' + x'*y 


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 1 
 - - Input: polyValue X (-345.3)
 - - Expected Output: -345.3
 - - Acutal Output: -345.3
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 2 
 - - Input: polyValue (Coef 4) (-345.3)
 - - Expected Output: 4.0
 - - Acutal Output: 4.0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 3
 - - Input: polyValue  ((Sum (Coef 1) (Sum (Prod X X) (Prod(Coef (-1)) (Prod X X))))) 999
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 1 
 - - Input: polyDegree (Prod (Coef 45) X)
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 2 
 - - Input: polyDegree (Sum (Prod (Prod X X) X) (Prod (Coef 2) X))
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDegree
 - - Test Case Number: 3 
 - - Input: polyDegree (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum X X) (Sum X X)))
 - - Expected Output: 2
 - - Acutal Output: 2
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 1 
 - - Input: polyDeriv (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum X X) (Sum X X)))
 - - Expected Output: Sum (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum (Coef 1) (Coef 1)) (Sum (Coef 1) (Coef 1)))) (Prod (Sum (Sum X X) (Sum X X)) (Sum (Sum (Coef 1) (Coef 0)) (Sum (Coef 0) (Coef 0))))
 - - Acutal Output: Sum (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum (Coef 1) (Coef 1)) (Sum (Coef 1) (Coef 1)))) (Prod (Sum (Sum X X) (Sum X X)) (Sum (Sum (Coef 1) (Coef 0)) (Sum (Coef 0) (Coef 0))))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 2 
 - - Input: polyDeriv (Prod X (Prod X (Prod X X)))
 - - Expected Output: Sum (Prod X (Sum (Prod X (Sum (Prod X (Coef 1)) (Prod X (Coef 1)))) (Prod (Prod X X) (Coef 1)))) (Prod (Prod X (Prod X X)) (Coef 1))
 - - Acutal Output: Sum (Prod X (Sum (Prod X (Sum (Prod X (Coef 1)) (Prod X (Coef 1)))) (Prod (Prod X X) (Coef 1)))) (Prod (Prod X (Prod X X)) (Coef 1))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyDeriv
 - - Test Case Number: 3 
 - - Input: polyDeriv (Prod (Coef 2) (Prod X X))
 - - Expected Output: Sum (Prod (Coef 2) (Sum (Prod X (Coef 1)) (Prod X (Coef 1)))) (Prod (Prod X X) (Coef 0))
 - - Acutal Output: Sum (Prod (Coef 2) (Sum (Prod X (Coef 1)) (Prod X (Coef 1)))) (Prod (Prod X X) (Coef 0))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 -}
