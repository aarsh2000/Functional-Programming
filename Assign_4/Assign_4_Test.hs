{- Assignment 4 Tests
 - Aarsh Patel
 - Novemeber 18th, 2018
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck propPolyListValue
          print "Performing Test 2: "
          quickCheck propPolyDegree
          print "Performing Test 3: "
          quickCheck propPolyDeriv
          print "Performing Test 4: "
          quickCheck propPolySum
          print "Performing Test 5: "
          quickCheck propPolyProd
          print "Performing Test 6: "
          quickCheck propPolySum'
          print "Performing Test 7: "
          quickCheck propPolyProd'
          
{- -----------------------------------------------------------------
 - Function: polyListValue
 - Property: The value of polynomial at f(1) is just equal to sum of all the terms in the polynomial
 - (1)+(1)^2...1^m == sum f(1)
 - Actual Test Result: Pass.
 -}
propPolyListValue :: [Integer] -> Bool
propPolyListValue  xs = polyListValue (PolyList xs) 1 == sum xs

{- -----------------------------------------------------------------
 - Function: polyListDegree
 - Property: The value length of the terms will always be greater than the degree of the polynomial
 - length f(a) > Degree f(a), where f(a) is some PolyList at a
 - Actual Test Result: Pass.
 -}
propPolyDegree :: [Integer] -> Bool
propPolyDegree [a] = True
propPolyDegree []  = True
propPolyDegree xs  = (toInteger (length xs)) > polyListDegree (PolyList xs) 

{- -----------------------------------------------------------------
 - Function: polyListDeriv
 - Property: The degree of the f'(x) will always be less than the f(x) 
 - Degree f(x) > Degree f'(x), where f(a) is some PolyList at a
 - Actual Test Result: Pass.
 -}
propPolyDeriv :: [Integer] -> Bool
propDeriv [x] = True
propDeriv []  = True
propPolyDeriv xs = polyListDegree (PolyList xs) >= polyListDegree (polyListDeriv (PolyList xs))

{- -----------------------------------------------------------------
 - Function: polyListSum
 - Property: Commutative law for summation of polynomials, evalutated at the same value
 - (f(a)+g(a)) = (g(a)+f(a)), where f(a) and g(a) is some PolyList at a
 - Actual Test Result: Pass.
 -}
propPolySum :: [Integer] -> [Integer] -> Integer -> Bool
propPolySum xs ys n = polyListValue (polyListSum (PolyList ys) (PolyList xs)) n ==  polyListValue (polyListSum (PolyList xs) (PolyList ys)) n

{- -----------------------------------------------------------------
 - Function: polyListProd
 - Property: Commutative law for product of polynomials, evaluated at the same value
 - (f(a)*g(a)) = (g(a)*f(a)), where f(a) and g(a) is some PolyList at a
 - Actual Test Result: Pass.
 -}
propPolyProd :: [Integer] -> [Integer] -> Integer -> Bool
propPolyProd xs ys n =  polyListValue (polyListProd (PolyList ys) (PolyList xs)) n ==  polyListValue (polyListProd (PolyList xs) (PolyList ys)) n

{- -----------------------------------------------------------------
 - Function: polyListSum
 - Property: Associate law for summation of polynomials, evalutated at the same value
 - (f(a)+g(a))+z(a) = (z(a)+f(a))+g(a), where f(a), g(a), and z(a) is some PolyList at a
 - Actual Test Result: Pass.
 -}
propPolySum' :: [Integer] -> [Integer] -> [Integer] -> Integer -> Bool
propPolySum' xs ys zs n= polyListValue (polyListSum (polyListSum (PolyList xs) (PolyList ys)) (PolyList zs)) n == polyListValue (polyListSum (polyListSum (PolyList zs) (PolyList ys)) (PolyList xs)) n

{- -----------------------------------------------------------------
 - Function: polyListSum
 - Property: Associate law for product of polynomials, evalutated at the same value
 - (f(a)*g(a))*z(a) = (z(a)*f(a))*g(a), where f(a), g(a), and z(a) is some PolyList at a
 - Actual Test Result: Pass.
 -}
propPolyProd' :: [Integer] -> [Integer] -> [Integer] -> Integer -> Bool
propPolyProd' xs ys zs n = polyListValue (polyListProd (polyListProd (PolyList xs) (PolyList ys)) (PolyList zs)) n == polyListValue (polyListProd (polyListProd (PolyList zs) (PolyList ys)) (PolyList xs)) n