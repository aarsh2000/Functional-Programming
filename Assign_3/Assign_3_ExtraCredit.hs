{- Assignment 3 Extra Credit
 - Name: Aarsh Patel
 - Date: November 1, 2018
 -}
module Assign_3_ExtraCredit where
macid = "patea80"

data PolyAlt a = Monomial a Integer
               | SumAlt (PolyAlt a) (PolyAlt a)
  deriving Show

--From Assign_3, needed for last two functions
data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show

{- -----------------------------------------------------------------
 - polyAltValue
 - -----------------------------------------------------------------
 - polyValue evaluates the polynomial at a given value n
 - This function recursively evaluates for n
 - Input: A polynomial (PolyAlt a), and evaluation number (a) 
 - Output: Returns number evaluated at polynomial (a)
 -}

polyAltValue :: Num a => PolyAlt a -> a -> a
polyAltValue (Monomial c 0) n = c                                          --Base case, define evaulation of constant, which is just constant
polyAltValue (Monomial c e) n = n*(polyAltValue (Monomial c (abs(e)-1)) n) --Defines evaluation at n for monomial with non-zero exponents, with recursive multiplication 
polyAltValue (SumAlt p1 p2) n = (polyAltValue p1 n) + (polyAltValue p2 n)  --Defines evaluation at n for sum of monomial, by recursively solving for n at each term                        

{- -----------------------------------------------------------------
 - polyAltDegree
 - -----------------------------------------------------------------
 - Given a monomial, or polynomial, this function will return the number of highest degree
 - This function determines the highest degree by recursively evaluating the polynomial/monomial
 - Input: A polynomial (PolyAlt a)
 - Output: Highest degree exponent (Integer)
 -}
polyAltDegree :: (Num a, Eq a) => PolyAlt a -> Integer
polyAltDegree (Monomial c e) = abs e                                                    --Base case, returns the exponent of monomial
polyAltDegree (SumAlt p1 p2) = if (valueOfp1 > valueOfp2) then valueOfp1 else valueOfp2 --Recursively compares highest exponent in a sum of monomials, and returns highest exponent
                            where                                                       --Made variables to make easier to read
                                valueOfp1 = polyAltDegree p1
                                valueOfp2 = polyAltDegree p2

{- -----------------------------------------------------------------
 - polyAltDeriv
 - -----------------------------------------------------------------
 - Given a monomial or polynomial, this function will return the derivative
 - This function recursively determines the derivative using laws of calculus
 - Input: A polynomial (PolyAlt a)
 - Output: Derivative of polynomial (PolyAlt a)
 -}

polyAltDeriv :: Num a => PolyAlt a -> PolyAlt a
polyAltDeriv (Monomial c 0) = Monomial 0 0                                --Base case, defines derivative of constant, which is 0
polyAltDeriv (Monomial c e) = Monomial (c*fromIntegral(abs e)) (abs(e)-1) --Defines derivative of monomial using law (c*e)X^(e-1)
polyAltDeriv (SumAlt p1 p2) = SumAlt (polyAltDeriv p1) (polyAltDeriv p2)  --Defines sumation rule of derivatives (x+y)' = x' + y'

{- -----------------------------------------------------------------
 - polyAltProd
 - -----------------------------------------------------------------
 - Given two monomial/polynomial, this fuction will return the product
 - This function recursively determines the product
 - Input: Two polynomials (PolyAlt a)
 - Output: Derivative of polynomial (PolyAlt a)
 -}

polyAltProd :: Num a => PolyAlt a -> PolyAlt a -> PolyAlt a
polyAltProd (Monomial c1 e1) (Monomial c2 e2) = Monomial (c1*c2) (abs e1 + abs e2)         --Coefficients are multipled, and exponents are added
polyAltProd (SumAlt p q) r                    = SumAlt (polyAltProd p r) (polyAltProd q r) --Applies it to a sum of polynomials by expanding, r(p*q) = r*p + r*q
polyAltProd r (SumAlt p q)                    = polyAltProd (SumAlt p q) r                 --Associative law, equivalent to line above

{- -----------------------------------------------------------------
 - polyAltNewton
 - -----------------------------------------------------------------
 - Given a monomial or polynomial, this function will return the root
 - This function recursively determines the derivative using Newton's method of approximation
 - Input: A polynomial (PolyAlt a), seed (a), and tolerence (a)
 - Output: An approximation of the root (a)
 -}

polyAltNewton :: (Fractional a, Ord a) => PolyAlt a -> a -> a -> a
polyAltNewton p s t = if abs (polyAltValue p s) < t then s else polyAltNewton p x t     --Checks if the polynomial at current seed is less than the tolerence
                  where x  = s - ((polyAltValue p s)/(polyAltValue (polyAltDeriv p) s)) --Formula for Newton's method of approximating roots, using polyAltValue, and polyAltDeriv

{- -----------------------------------------------------------------
 - polyToPolyAlt
 - -----------------------------------------------------------------
 - Given data in form Poly function converts polynomial to data in type PolyAlt
 - This function recursively determines the equivalent form of PolyAlt
 - Input: A polynomial (Poly a)
 - Output: A polynomial (PolyAlt a)
 -}
            
polyToPolyAlt  :: (Num a, Eq a) => Poly a -> PolyAlt a
polyToPolyAlt X         = Monomial 1 1                                     --Base case, X in form Poly is equal to Monomial 1 1 in form PolyAlt
polyToPolyAlt (Coef a)   = Monomial a 0                                    --Base case, Coef a in form Poly is equal to Monomial a 0 (no exponents) in form PolyAlt
polyToPolyAlt (Sum p q)  = SumAlt (polyToPolyAlt p) (polyToPolyAlt q)      --Converts Sum p q in form Poly to sumAlt p q in form PolyAlt, using recursion
polyToPolyAlt (Prod p q) = polyAltProd (polyToPolyAlt p) (polyToPolyAlt q) --Converts Prod pq in form Poly to polyAltProd p q in form PolyAlt, using recursion


{- -----------------------------------------------------------------
 - polyAltToPoly
 - -----------------------------------------------------------------
 - Given data in form PolyAlt function converts polynomial to data in type Poly
 - This function recursively determines the equivalent form of Poly
 - Input: A polynomial (PolyAlt a)
 - Output: A polynomial (Poly a)
 -}
polyAltToPoly :: (Num a, Eq a) => PolyAlt a -> Poly a
polyAltToPoly (Monomial c 0) = Coef c                                         --Base case, Monomial c 0 (constant) is equal to Coef c in form Poly
polyAltToPoly (Monomial c 1) = Prod (Coef c) X                                --Base case, Monomial c 1 is equal to Prod (Coef c) X in form Poly
polyAltToPoly (Monomial c e) = Prod X (polyAltToPoly (Monomial c (abs(e-1)))) --Converts a Monomial c e to form Poly, using recursion, so {X Prod X Prod X ...(e-1 times) Prod X Coef c}
polyAltToPoly (SumAlt p q)   = Sum (polyAltToPoly p) (polyAltToPoly q)        --Converts a sum of Monomials by breaking it down with the cases given above

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 1
 - - Input: polyAltValue (Monomial (-42) (-4)) 0.5
 - - Expected Output: -2.625
 - - Acutal Output: -2.625
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 2
 - - Input: polyAltValue (polyAltProd (Monomial 65 4) (Monomial 6 (-21))) 2
 - - Expected Output: 13086228480
 - - Acutal Output: 13086228480
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 3
 - - Input: polyAltValue (SumAlt (Monomial (-24) (-4)) (Monomial 3 5)) 2
 - - Expected Output: -288
 - - Acutal Output: -288
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltDegree
 - - Test Case Number: 1
 - - Input: polyAltDegree (polyAltProd (Monomial (-1) 3) (Monomial (-2) 5))
 - - Expected Output: 8
 - - Acutal Output: 8
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltDegree
 - - Test Case Number: 2
 - - Input: polyAltDegree (Monomial 5 (-9))
 - - Expected Output: 9
 - - Acutal Output: 9
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltDegree
 - - Test Case Number: 3
 - - Input: polyAltDegree (SumAlt (Monomial (-3) (1)) (polyAltProd (Monomial (-53) 3) (Monomial 35 6)))
 - - Expected Output: 9
 - - Acutal Output: 9
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltDeriv
 - - Test Case Number: 1 
 - - Input: polyAltDeriv (Monomial 3 (-4))
 - - Expected Output: Monomial 12 3
 - - Acutal Output: Monomial 12 3
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltDeriv
 - - Test Case Number: 2 
 - - Input: polyAltDeriv (SumAlt (Monomial 5 6) (Monomial (-3) 4))
 - - Expected Output: SumAlt (Monomial 30 5) (Monomial (-12) 3)
 - - Acutal Output: SumAlt (Monomial 30 5) (Monomial (-12) 3)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltDeriv
 - - Test Case Number: 3 
 - - Input: polyAltDeriv (polyAltProd (Monomial 3 5) (SumAlt (Monomial (-4) 7) (Monomial 1 2)))
 - - Expected Output: SumAlt (Monomial (-144) 11) (Monomial 21 6)
 - - Acutal Output: SumAlt (Monomial (-144) 11) (Monomial 21 6)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltProd
 - - Test Case Number: 1 
 - - Input: polyAltProd (Monomial 3 (-4)) (Monomial 2 (-3))
 - - Expected Output: Monomial 6 7
 - - Acutal Output: Monomial 6 7
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltProd
 - - Test Case Number: 2 
 - - Input: polyAltProd (Monomial (-2) (-3)) (SumAlt (Monomial 1 2) (Monomial 1 (-3)))
 - - Expected Output: SumAlt (Monomial (-2) 5) (Monomial (-2) 6)
 - - Acutal Output: SumAlt (Monomial (-2) 5) (Monomial (-2) 6)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltProd
 - - Test Case Number: 3 
 - - Input: polyAltProd (SumAlt (Monomial (-2) (-3)) (Monomial 4 5)) (SumAlt (Monomial 1 2) (Monomial 1 (-3)))
 - - Expected Output: SumAlt (SumAlt (Monomial (-2) 5) (Monomial (-2) 6)) (SumAlt (Monomial 4 7) (Monomial 4 8))
 - - Acutal Output: SumAlt (SumAlt (Monomial (-2) 5) (Monomial (-2) 6)) (SumAlt (Monomial 4 7) (Monomial 4 8))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltNewton
 - - Test Case Number: 1 
 - - Input: polyAltNewton (SumAlt (Monomial (-3) 2) (Monomial 4 3)) 2 0.000001
 - - Expected Output: 0.7500000003901208
 - - Acutal Output: 0.7500000003901208
 - -----------------------------------------------------------------
 - - Function: polyAltNewton
 - - Test Case Number: 2 
 - - Input: polyAltNewton (SumAlt (Monomial (-2) (-12)) (Monomial 234 (-33))) 10002 4
 - - Expected Output: 0.8691700180705625
 - - Acutal Output: 0.8691700180705625
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltNewton
 - - Test Case Number: 3 
 - - Input: polyAltNewton (SumAlt (Monomial 1 1) (Monomial 1 1)) 999999 0.00001
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyToPolyAlt
 - - Test Case Number: 1 
 - - Input: polyToPolyAlt (Sum (Prod X (Prod X (Prod (Coef 3) X))) (Prod X (Coef (-547))))
 - - Expected Output: SumAlt (Monomial 3 3) (Monomial (-547) 1)
 - - Acutal Output: SumAlt (Monomial 3 3) (Monomial (-547) 1)
 - -----------------------------------------------------------------
 - - Function: polyToPolyAlt
 - - Test Case Number: 2 
 - - Input: polyToPolyAlt (Prod (Sum X (Prod (Coef (-3)) (Prod (Sum X (Coef 2)) X))) (Prod X (Coef (4))))
 - - Expected Output: SumAlt (Monomial 4 2) (SumAlt (Monomial (-12) 3) (Monomial (-24) 2))
 - - Acutal Output: SumAlt (Monomial 4 2) (SumAlt (Monomial (-12) 3) (Monomial (-24) 2))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyToPolyAlt
 - - Test Case Number: 3 
 - - Input: polyToPolyAlt  (Prod X (Prod X (Prod X X)))
 - - Expected Output: Monomial 1 4
 - - Acutal Output: Monomial 1 4
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 1 
 - - Input: polyAltToPoly (Monomial 1 4)
 - - Expected Output: Prod X (Prod X (Prod X (Prod (Coef 1) X)))
 - - Acutal Output: Prod X (Prod X (Prod X (Prod (Coef 1) X)))
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 2 
 - - Input: polyAltToPoly (SumAlt (Monomial 4 2) (SumAlt (Monomial (-12) 3) (Monomial (-24) 2)))
 - - Expected Output: Sum (Prod X (Prod (Coef 4) X)) (Sum (Prod X (Prod X (Prod (Coef (-12)) X))) (Prod X (Prod (Coef (-24)) X)))
 - - Acutal Output: Sum (Prod X (Prod (Coef 4) X)) (Sum (Prod X (Prod X (Prod (Coef (-12)) X))) (Prod X (Prod (Coef (-24)) X)))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyAltToPoly
 - - Test Case Number: 3 
 - - Input: polyAltToPoly (SumAlt (Monomial 3 3) (Monomial (-547) 1))
 - - Expected Output: Sum (Prod X (Prod X (Prod (Coef 3) X))) (Prod (Coef (-547)) X)
 - - Acutal Output: Sum (Prod X (Prod X (Prod (Coef 3) X))) (Prod (Coef (-547)) X)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 -}

