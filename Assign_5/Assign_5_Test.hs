{- Assignment 5 Tests
 - Name: Aarsh Patel
 - Date: 12/1/2018
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck propDefiniteIntegral1
          print "Performing Test 2: "
          quickCheck propDefiniteIntegral2
          print "Performing Test 3: "
          quickCheck propDefiniteIntegral3
          print "Performing Test 4: "
          quickCheck propFunH1
          print "Performing Test 5: "
          quickCheck propFunH2
          print "Performing Test 6: "
          quickCheck propFunK1
          print "Performing Test 7: "
          quickCheck propFunK2

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