{- Assignment 4
 - Aarsh Patel
 - Novemeber 18th, 2018
 -}
module Assign_4 where

macid :: String
macid = "patea80"


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a) 
            | Prod (Poly a) (Poly a)
            deriving Show

newtype PolyList a = PolyList [a] deriving Show

{- -----------------------------------------------------------------
 - getPolyList
 - -----------------------------------------------------------------
 - This function is responsible for reading a file of polynomials in 
 - in the standard form where each line has an integer (each integer
 - represents a coefficient). The line corresponds to the exponent 
 - of each term. Once getPolyList reads the file, it converts the
 - Integers into a list, inside a PolyList.
-}
getPolyList :: FilePath -> IO (PolyList Integer)
getPolyList file = do                                -- Beggining of Do block
                    inp <- readFile file             -- Assigns the readed files to variable inp
                    let                              -- let statement for getLine and readLine
                        getLine = lines inp          -- finds the input per line
                        readLine = map read getLine  -- maps the each line in the file
                    return (PolyList(readLine))      -- returns the list in PolyList Integer form 


{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - This function is responsible for evaluating the polynomial at some
 - value at n. Furthermore the function uses Horner's method, so
 - the function is evaluated like, 
 - a + a1 * x1 + a2 *x^2+...+am + x^m = a + x*(a1 + x(a2+...+x(am)))
 -}
polyListValue :: Num a => PolyList a -> a -> a
polyListValue (PolyList []) n     = 0
polyListValue (PolyList (x:xs)) n = x + n*(polyListValue (PolyList xs) n) --Formula for Horner's method 



{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - This function is responsible for determining the highest degree in 
 - a PolyList. This is done simply by determining the length of the 
 - list. The length is subtracted by one, because the first element 
 - in the PolyList is a constant (degree of 0).
 -}
polyListDegree :: (Num a, Eq a) => PolyList a -> Integer
polyListDegree (PolyList a) = toInteger((length a)-1) --Returns one less of the length of the list in PolyList


{- -----------------------------------------------------------------
 - polyListDeriv
 - -----------------------------------------------------------------
 - This is function is responsible for determining the derivative of
 - of a polynomial in the form of PolyList. The derivative is determined
 - by removing the first element of the list (since the derivative of
 - of a constant is 0). The rest of the list is then multiplied by its
 - element, giving the derivative. The list is multiplied with the 
 - zipWith function. The second list which is generated from [1..length xs]
 - and then with map each element is converted to the form [a] with functions 
 - fromEnum, amd fromIntegeral. (fromEnum allows the list to be generated
 - from the Enum class, without actually habing to add 'Enum a' to the header)
 -}
polyListDeriv :: (Num a, Eq a) => PolyList a -> PolyList a
polyListDeriv (PolyList [])     = PolyList []                                                                                        -- Derivative of empty list is empty list                                   
polyListDeriv (PolyList (x:xs)) = PolyList (zipWith (*) xs $ map (\x -> (fromIntegral (fromEnum x))) [1..(fromIntegral(length xs))]) -- multiplies xs with the list generated [1..length xs], giving the derivative


{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - This function is responsible for finding the sum of two PolyLists.
 - The function checks which polyList has a bigger length, to apply
 - the replicate function to the shorter list. Both of the lists become
 - the same length by adding '0', by the difference in length of the 
 - two lists. Once both lists are the same length, each element is added
 - to the corresponding element on the other list using the zipWith 
 - function. Finally, since some cases can result in elements completely 
 - canceling each other out, a helper function 'simplfy' is called, which
 - simplfies the list. 
 -}
polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList []) b             = b                                                                                                                                   --Case when one PolyList is empty              
polyListSum a (PolyList [])             = a                                                                                                                                   --Case when one PolyList is empty      
polyListSum (PolyList p1) (PolyList q1) = if      (length p1 > length q1)               then  PolyList $ simplfy $ zipWith (+) p1 (q1 ++replicate (length p1 - length q1)  0) --This is the case in which the first list is longer than the second
                                          else if (length p1 < length q1)               then  PolyList $ simplfy $ zipWith (+) (p1 ++replicate (length q1 - length p1)  0) q1 --This is the case in which the second list is longer than the first
                                          else    PolyList $ simplfy $ zipWith (+) p1 q1                                                                                      --This is the case in which both lists have the same length
                                          
simplfy ::(Num a, Eq a) => [a] -> [a]                                                    --This function is used by PolyListSum to simplfy the lists
simplfy [] = []                                                                          --Cases in which empty list 
simplfy x = if (head(reverse x)) == 0 then (simplfy(reverse(drop 1 (reverse x)))) else x --This function recursively drops the last 0 (removes trailing 0s) 

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - This function is responsible for finding the product of two PolyLists.
 - The function uses basic laws of multiplication, such that 
 - a * (b + c) = a * b + a * c 
 - and (a + b) * (c + d) =  (a + b) * c +  (a + b) * d =  a*c + b*c + a*d + b*d
 - In order to implent the properties described above, I used the 
 - predefined map funciton, and the polyListSum function.
 -}
polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) _                   = PolyList []                                                                                              -- Multiplying with empty list will result in a empty list          
polyListProd _ (PolyList [])                   = PolyList []                                                                                              -- Multiplying with empty list will result in a empty list    
polyListProd (PolyList [y]) (PolyList q1)      = PolyList ((map (\x->y*x) q1))                                                                            -- Uses rule, a*(b+c) = a*b +a*c (using map function)
polyListProd (PolyList (x:xs)) (PolyList q1)   = polyListSum (polyListProd (PolyList [x]) (PolyList q1))  (polyListProd  (PolyList xs) (PolyList (0:q1))) -- Uses rule (a+b)*(c+d) = (a+b)*c + (a+b)*d = a*c + b*c + a*d + b*d (this is done recursively)


{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - This function is responsible for converting the PolyList form
 - of a polynomial to Poly.This is done recursively, using Horner's
 - method, and a few simple base cases. This function also uses
 - predefined types form Poly, including Coef, Sum, and Prod.
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList [a])    = Coef a                                               --Base case, converts a constant to Coef
polyListToPoly (PolyList [])     = Coef 0                                               --Base case, converts a empty list to a constant of 0 (Coef 0)
polyListToPoly (PolyList (x:xs)) = Sum (Coef x) $ Prod X (polyListToPoly (PolyList xs)) --Recursive implementation of Horner's method

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - This function is responsible for converting the Poly form of a
 - polynomial to PolyList. This is done recursively, with the help
 - of a polyListSum and polyListProd, functions which were defined
 - above. 
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList X          = PolyList [0,1]                                     -- Base case, converts degree of X into correct list form
polyToPolyList (Coef a)   = PolyList [a]                                       -- Base case, converts constant into first element of the list
polyToPolyList (Sum a b)  = polyListSum (polyToPolyList a)  (polyToPolyList b) -- Recursively finds the sum, using polyListSum
polyToPolyList (Prod a b) = polyListProd (polyToPolyList a) (polyToPolyList b) -- Recursively finds the product, using polyListProd

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: getPolyList
 - - Test Case Number: 1 
 - - Input: getPolyList "emptyFile.txt"
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: getPolyList
 - - Test Case Number: 2 
 - - Input: getPolyList "fileA.txt"
 - - Expected Output: PolyList [1,2,3,4]
 - - Acutal Output: PolyList [1,2,3,4]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: getPolyList
 - - Test Case Number: 3
 - - Input: getPolyList "fileB.txt"
 - - Expected Output: PolyList [-1,2-3,0,4]
 - - Acutal Output: PolyList [-1,2-3,0,4]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 1 
 - - Input: polyListValue (PolyList []) 1
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 2 
 - - Input: polyListValue (PolyList [1,-2,3]) 0.5
 - - Expected Output: 0.75
 - - Acutal Output: 0.75
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 3 
 - - Input: polyListValue (PolyList [1,2,3]) 0
 - - Expected Output: 1
 - - Acutal Output: 1
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 1 
 - - Input: polyListDegree (PolyList [1,2,3,4])
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 2 
 - - Input: polyListDegree (PolyList [])
 - - Expected Output: -1
 - - Acutal Output: Sum -1
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 3 
 - - Input: polyListDegree (PolyList [0,02,3,-44,0.5])
 - - Expected Output: 4
 - - Acutal Output: 4
  - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 1 
 - - Input: polyListDeriv (PolyList [1,0,2,3])
 - - Expected Output: PolyList [0,4,9]
 - - Acutal Output: PolyList [0,4,9]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 2 
 - - Input: polyListDeriv (PolyList [])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListDeriv
 - - Test Case Number: 3 
 - - Input: polyListDeriv (PolyList [1,-2,0.5,-3/2])
 - - Expected Output:PolyList [-2.0,1.0,-4.5]
 - - Acutal Output: PolyList [-2.0,1.0,-4.5]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 1 
 - - Input: polyListSum (PolyList []) (PolyList [1,2,3])
 - - Expected Output: PolyList [1,2,3]
 - - Acutal Output: PolyList [1,2,3]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 2 
 - - Input: polyListSum (PolyList [-1,-2,-3]) (PolyList [1,2,3]) 
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 3 
 - - Input: polyListSum (PolyList [51,-2,33]) (PolyList [-341,0,13])
 - - Expected Output: PolyList [-290,-2,46]
 - - Acutal Output:PolyList [-290,-2,46]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 1 
 - - Input:polyListProd (PolyList []) (PolyList [1,2,3])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 2 
 - - Input: polyListProd (PolyList [1,2]) (PolyList [1,2,3])
 - - Expected Output: PolyList [1,4,7,6]
 - - Acutal Output: PolyList [1,4,7,6]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 3 
 - - Input: polyListProd (PolyList [-105,322]) (PolyList [1,-2,233])
 - - Expected Output: PolyList [-105,532,-25109,75026]
 - - Acutal Output: PolyList [-105,532,-25109,75026]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 1 
 - - Input: polyListToPoly (PolyList [-5])
 - - Expected Output:Coef (-5)
 - - Acutal Output: Coef (-5)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 2 
 - - Input: polyListToPoly (PolyList [])
 - - Expected Output: Coef 0
 - - Acutal Output:Coef 0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 3 
 - - Input: polyListToPoly (polyListProd (PolyList [1,2,3]) (PolyList [4,5,6]))
 - - Expected Output: Sum (Coef 4) (Prod X (Sum (Coef 13) (Prod X (Sum (Coef 28) (Prod X (Sum (Coef 27) (Prod X (Coef 18))))))))
 - - Acutal Output: Sum (Coef 4) (Prod X (Sum (Coef 13) (Prod X (Sum (Coef 28) (Prod X (Sum (Coef 27) (Prod X (Coef 18))))))))
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 1 
 - - Input: polyToPolyList $Sum (Coef 4) (Prod X (Sum (Coef 13) (Prod X (Sum (Coef 28) (Prod X (Sum (Coef 27) (Prod X (Coef 18))))))))
 - - Expected Output: PolyList [4,13,28,27,18]
 - - Acutal Output:
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 2 
 - - Input: polyToPolyList (Coef 0)
 - - Expected Output: PolyList [0]
 - - Acutal Output: PolyList [0]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 3 
 - - Input: polyToPolyList (Prod X (Coef 9))
 - - Expected Output: PolyList [0,9]
 - - Acutal Output: PolyList [0,9]
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 -}

{-QuickCheck Test
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
-}