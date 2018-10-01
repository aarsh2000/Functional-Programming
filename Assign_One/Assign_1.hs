{- Assignment 1
 - Name: Aarsh Patel
 - Date: September 30th, 2018
 -}
module Assign_1 where
macid :: String
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
 - cubicS
 - -----------------------------------------------------------------
 - The function cubicS determines S
 ->Takes in 2 inputs: Q, R (all type Double)
 ->Computes the formula: (R+(Q^3+R^2)^(1/2))^(1/3) (type Double)
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubicRoot(r+sqrt(cubicDisc q r))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - The function cubicT determines T
 ->Takes in 2 inputs: Q, R (all type Double)
 ->Computes the formula: (R-(Q^3+R^2)^(1/2))^(1/3) (type Double)
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubicRoot(r-sqrt(cubicDisc q r))

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
            (-1)*((abs num)**(1/3)) --Takes out the negative into the front so -(num)**(1/3)
        else --If num is positive it just does regular cube root 
            (num)**(1/3)

{- -----------------------------------------------------------------
 - cubicRealSolutions 
 - -----------------------------------------------------------------
 - The function determines most solutions to cubic function, by using all of the functions defined above
 ->Takes in 4 co-efficients as inputs: a, b, c, d (type Double)
 ->Capable of computing 3 outputs: x1,x2,x3  (type List of Double(s))
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d = 
    let
    q = cubicQ a b c
    r = cubicR a b c d
    s = cubicS q r 
    t = cubicT q r
    x1 = (s+t)- b/(3*a) 
    in if(cubicDisc q r > 0)
        then do
        [x1]
        else if(abs (cubicDisc q r) < 1e-6)
            then do 
            let x2 = (-1*(s+t)/2) - (b/(3*a)) + (sqrt(3)/2 *(s-t))
            let x3 = (-1*(s+t)/2) - (b/(3*a)) - (sqrt(3)/2 * (s-t))
            [x1,x2,x3]
            else 
            []
                
{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}

{-
-Function:cubicRealSolutions
-Test Case Number: 1 (Discriminant>0)
-Input: 1 6 3 1
-Expected Output: [-5.486]
-Actual Output: [-5.48641676436457]
-}

{-
-Function:cubicRealSolutions
-Test Case Number: 2 (Discriminant==0)
-Input: 1 (-1) (-1) 1
-Expected Output: [-1.0,1.0,1.0]
-Actual Output:[-1.0,1.0,1.0]
-}

{-
-Function:cubicRealSolutions
-Test Case Number: 3 (Discriminant<0)
-Input: 1 (-5) 5 1
-Expected Output: []
-Actual Output:[]
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
-Function:cubicS
-Test Case Number: 1
-Input: 1 2
-Expected Output: 1.618 
-Actual Output: 1.618033988749895
-}

{-
-Function:cubicT
-Test Case Number: 1
-Input: 1 2 
-Expected Output: -0.618
-Actual Output: -0.618033988749895
-}

{-
-Function:cubicRoot
-Test Case Number: 1
-Input: (-27)
-Expected Output: -3.0
-Actual Output: -3.0
-}
