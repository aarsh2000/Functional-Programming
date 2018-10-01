{- Assignment 1
 - Name: Aarsh Patel    
 - Date: September 30th, 2018
 -}

module Assign_1_ExtraCredit where
import Data.Complex 
-- see https://www.stackage.org/haddock/lts-8.24/base-4.9.1.0/Data-Complex.html

macid = "patea80"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - The function cubicQ determines Q 
 ->Takes in 3 inputs: a, b, c (all type Double)
 ->Computes the formula = (3ac-b^2)/(9a^2) (type Double)
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (3*a*c-(b^2))/(9*(a^2))
 
 {- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - The function cubicR determines R 
 ->Takes in 4 inputs: a, b, c, d (all type Double)
 ->Computes the formula:(9abc-27(a^2)d-2b^3)/(54a^3)(type Double)
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (9*a*b*c-27*(a^2)*d-2*(b^3))/(54*(a^3))
 
{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 -The function cubicDisc determines the discriminant 
 ->Takes in 2 inputs: Q, R (all type Double)
 ->Computes the formula: Q^3 + R^2 (type Double)
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = q^3 + r^2

{- -----------------------------------------------------------------
 - toComplex
 - -----------------------------------------------------------------
 - The function takes real numbers and puts them in complex type
 ->Takes in 1 input: x (type Double)
 ->Returns value in Complex Double form
 -}

toComplex :: Double -> Complex Double
toComplex x = x :+ 0


{- -----------------------------------------------------------------
 - cubicComplexS
 - -----------------------------------------------------------------
 - The function cubicComplexS determines S using complex arithmetic
 ->Takes in 2 inputs: Q, R (all type Double)
 ->Computes the formula: (R+(Q^3+R^2)^(1/2))^(1/3) (type Complex Double)
 -}
cubicComplexS :: Double -> Double -> Complex Double
cubicComplexS q r = 
    if(cubicDisc q r < 0) --checks if discriminant is negative
        then
            cubicRoot(r+sqrt(-1*(cubicDisc q r))) :+ 0 --if negative, makes discriminant positive and converts it to complex form
        else cubicRoot(r+sqrt(cubicDisc q r)) :+ 0 --leaves it as is, and converts to complex form


{- --
---------------------------------------------------------------
 - cubicComplexT
 - -----------------------------------------------------------------
 - The function cubicComplexT determines T using complex arithmetic
 ->Takes in 2 inputs: Q, R (all type Double)
 ->Computes the formula: (R-(Q^3+R^2)^(1/2))^(1/3) (type Complex Double)
 -}
cubicComplexT :: Double -> Double -> Complex Double
cubicComplexT q r = 
    if(cubicDisc q r < 0)--checks if discriminant is negative
        then
            cubicRoot(r-sqrt(-1*(cubicDisc q r))) :+ 0 --if negative, makes discriminant positive and converts it to complex form
        else cubicRoot(r-sqrt(cubicDisc q r)) :+ 0 --leaves it as is, and converts to complex form

{- -----------------------------------------------------------------
 - cubicRoot
 - -----------------------------------------------------------------
 - The function cubicRoot determines cubic root
 ->Takes in 1 input: num (type Double)
 ->Computes the formula: (R-(Q^3+R^2)^(1/2))^(1/3) (type Double)
 -}
 
cubicRoot :: Double -> Double
cubicRoot num = 
     if(num < 0) --Since (-num)**(1/3) == NaN
         then
             (-1)*((abs num)**(1/3))--Takes out the negative into the front so -(num)**(1/3)
         else
             (num)**(1/3)--If num is positive it just does regular cube root 
 

{- -----------------------------------------------------------------
 - cubicComplexSolutions 
 - -----------------------------------------------------------------
 - The function determines all solutions to cubic function, by using all of the functions defined above and complex arthemthic 
 ->Takes in 4 co-efficients as inputs: a, b, c, d (type Double)
 ->Capable of computing 3 outputs: x1,x2,x3  (type List of Complex Double(s))
 -}


cubicComplexSolutions :: Double -> Double -> Double -> Double -> [Complex Double]
cubicComplexSolutions a b c d = 
     let
     q = cubicQ a b c
     r = cubicR a b c d
     s = cubicComplexS q r 
     t = cubicComplexT q r
     x1 = (s+t)- toComplex (b/(3*a))
     x2 = (-1*(s+t)/2) - toComplex(b/(3*a)) + (sqrt(3)/2 *(s-t))
     x3 = (-1*(s+t)/2) - toComplex(b/(3*a)) - (sqrt(3)/2 * (s-t))
     in [x1,x2,x3] 

               
{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}

{-
-Function:cubicComplexSolutions
-Test Case Number: 1 (Discriminant>0)
-Input: 1 6 3 1
-Expected Output: [(-5.486) :+ 0.0,0.0 :+ 0.0,0.0 :+ (-0.0)]
-Actual Output: [(-5.48641676436457) :+ 0.0,8.427493160004446e-2 :+ 0.0,(-0.5978581672354748) :+ (-0.0)]
-Ignore x2, and x3 they are not actual solutions
-}

{-
-Function:cubicComplexSolutions
-Test Case Number: 2 (Discriminant==0)
-Input: 1 (-1) (-1) 1
-Expected Output: [(-1.0) :+ 0.0,1.0 :+ 0.0,1.0 :+ (-0.0)]
-Actual Output:[(-1.0) :+ 0.0,1.0 :+ 0.0,1.0 :+ (-0.0)]
-}

{-
-Function:cubicComplexSolutions
-Test Case Number: 3 (Discriminant<0)
-Input: 1 (-5) 5 1
-Expected Output: [1.689:+ 0.0,3.481 :+ 0.0,(-0.17) :+ (-0.0)]
-Actual Output:[1.6444329093573444 :+ 0.0,3.503017881770872 :+ 0.0,(-0.14745079112821657) :+ (-0.0)]
-}


{-
-Function:cubicQ
-Test Case Number: 1
-Input: 1 2 3
-Expected Output: 0.55 
-Actual Output: 0.5555555555555556
-}

{-
-Function:cubicR
-Test Case Number: 1
-Input: 1 2 3 4
-Expected Output: -1.29 
-Actual Output: -1.2962962962962963
-}


{-
-Function:cubicDisc
-Test Case Number: 1
-Input: 2 3
-Expected Output: 17.0
-Actual Output: 17.0
-}

{-
-Function:toComplex
-Test Case Number: 1
-Input: 5
-Expected Output: 5 :+ 0.0
-Actual Output: 5 :+ 0.0
-}

{-
-Function:cubicComplexS
-Test Case Number: 1
-Input: 1 2
-Expected Output: 1.618 :+ 0.0
-Actual Output: 1.618033988749895 :+ 0.0
-}

{-
-Function:cubicComplexT
-Test Case Number: 1
-Input: 1 2 
-Expected Output: (-0.618) :+ 0.0
-Actual Output: (-0.618033988749895) :+ 0.0
-}

{-
-Function:cubicRoot
-Test Case Number: 1
-Input: (-27)
-Expected Output: -3.0
-Actual Output: -3.0
-}
