{- Assignment 5
 - Name: Aarsh Patel
 - Date: 12/1/2018
 -}
module Assign_5 where

macid :: String
macid = "patea80"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - With the given trapezoid rule, I rewrote the rule such that, 
 - Integeral from a to b = { [f(a) + f(x0)] + [f(x0) + f(x1)] ... [f(x(n-1)) + f(b)] } * (1/2) * (b-a)/n (I factored out the 1/2 from each term)
 - to easily compute this formula I further simplfied the given expression above such that,
 - Integeral form a to b = [f(a) + 2*f(x0) + 2*f(x1) + 2*f(x2) ... f(b)] * (1/2) * (b-a)/n
 - As you can see, the second formula is easier to compute with list comprehension
 - In order to integrate, I first created list containing,
 - [x1,x2,...x(n-1)], (note x0, and xn are not included)
 - then for each value in the list, I evaluated the function, and multiplied it by 2 (as indicated from the simplfied expression of the trapazoid rule)
 - so, [2*g(x1),2*g(x2),...2*g(x(n-1))]
 - after mapping each element, the total sum of all the elements in the list was determined
 - in the final step g(x0) and g(xn) (also known as g(a) and g(b)) were added to the total sum,
 - and remaining formula was applied, which is multiplying the total sum by (b-a)/2*n
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n =  if (b==a) then 0 else (g(a) + g(b) + (sum (map (\x -> 2*g(x)) li)))* (deltaX*(1/2)) --if a and b are the same, there is no need to compute an integral, thus 0 is returned, else the integral is computed using the method described above
                                where 
                                    li = [a+deltaX, a+2*deltaX .. b-deltaX]                                     -- li is generated list which formulates a list, [x1,x2,...x(n-1)], (note x0, and xn are not included)
                                    deltaX =((b-a)/(fromIntegral n))                                            -- deltaX represents the change in x, calculated by the given formula

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Since x^(1/n) is always above x^n, we can integrate the upper function (x^(1/n))
 - and subtract it with the integration of the bottom function (x^n)
 - Since the function definiteIntegral depends on aproximation of the area,
 - 10000 partitions were chosen to get accurate answer, with minimal runtime
 - Note that the intersection of x^(1/n) and x^n is always between 0 and 1, thus
 - the values of a and b are 0 and 1 respectively 
 -}
funH :: Integer -> Double
funH n = if n <= 0 then error"n<=0" else (definiteIntegral 0 1 (\x -> x**(1/(fromIntegral n))) 10000) - (definiteIntegral 0 1 (\x -> x^n) 10000)  --if n is less than or equal to 0 then an error is returned as specified else the functions finds the difference in area, as described above

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - The function n^x is always positive, so to determine the area
 - between -1 and 1 from the axis we can just integrate 
 - n^x from -1 to 1 using the function definiteIntegral described above
 - Since the function definiteIntegral depends on aproximation of the area,
 - 10000 partitions were chosen to get accurate answer, with minimal runtime
 -}
funK :: Double -> Double
funK n = if n<= 0 then error"n<=0" else (definiteIntegral (-1) 1 (\x -> n**x) 10000)  --if n is less than or equal to 0 then an error is returned as specified else the functions finds the area of n^x from -1 to 1, as described above

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 1
 - - Input: definiteIntegral 2 2 (\x -> x) 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 2
 - - Input: definiteIntegral (-1000) (1000) (\x -> x**3) 9999
 - - Expected Output: 0.0
 - - Acutal Output: 1.2190416581941397e-2
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 3
 - - Input: definiteIntegral (-50) (50) (\x -> x**2) 100
 - - Expected Output: 83350.0
 - - Acutal Output: 83350.0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 4
 - - Input: definiteIntegral (-0.001) (0) (\x -> sin x) 100
 - - Expected Output: -5e-7
 - - Acutal Output: -4.999999583291671e-7
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 5
 - - Input:  definiteIntegral (5) (0) (\x -> 3) 100
 - - Expected Output: -15.0
 - - Acutal Output: -15.0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 1
 - - Input: funH 100
 - - Expected Output: 0.98019
 - - Acutal Output:0.980193643731048798
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 2
 - - Input: funH 4
 - - Expected Output: 0.5999
 - - Acutal Output: 0.5999998197666774
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 3
 - - Input: funH 0
 - - Expected Output: error 
 - - Acutal Output: error
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 4
 - - Input: funH (-99)
 - - Expected Output: error
 - - Acutal Output: error
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 5
 - - Input:  funH 1000000000000000000
 - - Expected Output: 0.99999
 - - Acutal Output: 0.99999
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 1
 - - Input: funK 100
 - - Expected Output: 21.7126
 - - Acutal Output: 21.71255263828031
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 2
 - - Input: funK 4
 - - Expected Output: 2.7056
 - - Acutal Output: 2.7050532018454065
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 3
 - - Input: funK 0
 - - Expected Output: error 
 - - Acutal Output: error
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 4
 - - Input: funK (-99)
 - - Expected Output: error
 - - Acutal Output: error
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 5
 - - Input: funK 1000000000000000000
 - - Expected Output: 2.4127e16
 - - Acutal Output: 2.4127472600373572e16
 - -----------------------------------------------------------------
 -}

{- QuickCheck test

{- -----------------------------------------------------------------
 - Function: definiteIntegral
 - Property: When evaluating an even function, the integral from -n to n 
 - is equivalent by evaluating twice the integral from 0 to n
 - (this is a general property of integrals)
 - Note, definiteIntegral returns floating point number, thus
 - to compare two values, epsilon comparison is used 
 - Actual Test Result: Pass.
 -}          

propDefiniteIntegral1 :: Integer -> Bool
propDefiniteIntegral1 n = if n<=1 then True else abs ( 2*(definiteIntegral 0 1 (\x -> x^2) n) - (definiteIntegral (-1) 1 (\x -> x^2) n) )< 1

{- -----------------------------------------------------------------
 - Function: definiteIntegral
 - Property: When evaluating an even function, the integral from -n to n 
 - is equivalent by evaluating the negation of the integral from n to -n
 - (this is a general property of integrals)
 - Note, definiteIntegral returns floating point number, thus
 - to compare two values, epsilon comparison is used 
 - Actual Test Result: Pass.
 -}   

propDefiniteIntegral2 :: Double -> Double -> Bool
propDefiniteIntegral2 a b = if a==b then True else abs ( (definiteIntegral a b (\x -> x) 10) - (-1)*(definiteIntegral b a (\x -> x) 10) )< 0.001

{- -----------------------------------------------------------------
 - Function: definiteIntegral
 - Property: When evaluating an even function, the integral for a constant
 - is equivalent to (b-a)*c
 - (this is a general property of integrals)
 - Note, definiteIntegral returns floating point number, thus
 - to compare two values, epsilon comparison is used 
 - Actual Test Result: Pass.
 -}   

propDefiniteIntegral3 :: Double -> Double -> Double -> Bool
propDefiniteIntegral3 a b c = abs( (definiteIntegral a b (\x -> c) 10) - (b-a)*c ) < 0.01

{- -----------------------------------------------------------------
 - Function: funH
 - Property: If funH finds the area between some function x^(1/n)
 - and x^n, from the interval 0 to 1, then the area between x^(1/n)
 - and x axis should be greater
 - Actual Test Result: Pass.
 -}   

propFunH1 :: Integer -> Bool
propFunH1 n = if n<= 0 then True else (definiteIntegral 0 1 (\x -> x**(1/(fromIntegral n))) 1000) > funH n

{- -----------------------------------------------------------------
 - Function: definiteIntegral
 - Property: As n approaches infinity, the integration of funH approaches 1,
 - thus funH is always less than 1 
 - Actual Test Result: Pass.
 -}   

propFunH2 :: Integer -> Bool
propFunH2 n = if n<=0 then True else funH n < 1

{- -----------------------------------------------------------------
 - Function: definiteIntegral
 - Property: The integration of n^x and (1/n)^x is the same between 
 - the intervals of -1 to 1 
 - Note, funK returns floating point number, thus
 - to compare two values, epsilon comparison is used 
 - Actual Test Result: Pass.
 -} 

propFunK1 :: Integer -> Bool
propFunK1 n = if n<=0 then True else abs (funK (1/(fromIntegral n)) - funK (fromIntegral n))< 0.001

{- -----------------------------------------------------------------
 - Function: definiteIntegral
 - Property: n^x, when n>=0 is always postive, thus 
 - the integration of n^x should always be postive
 - Actual Test Result: Pass.
 -} 

propFunK2 :: Double -> Bool
propFunK2 n = if n<=0 then True else (funK n) > 0


-}