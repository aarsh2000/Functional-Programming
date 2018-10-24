{- Assignment 2
 - Name: Aarsh Patel
 - Date: October 19th, 2018
 -}
module Assign_2 where

macid :: String
macid = "patea80"

type Vector = (Double,Double,Double)

{- -----------------------------------------------------------------
 - vecZero
 - -----------------------------------------------------------------
 - Description: vecZero takes in no inputs and returns a vector (0,0,0) (Vector)
 -}
vecZero :: Vector
vecZero = (0,0,0)

{- -----------------------------------------------------------------
 - vecScalarProd
 - -----------------------------------------------------------------
 - Description: vecScalarProd takes in a multiple, c (Double),
 - and a vector (x,y,z) (Vector), and returns the scalar product (Vector)
 -}
vecScalarProd :: Double -> Vector -> Vector
vecScalarProd c (x,y,z) = (x*c,y*c,z*c)

{- -----------------------------------------------------------------
 - vecSum
 - -----------------------------------------------------------------
 - Description: vecSum takes in two vectors (Vector) as input, and 
 - returns the vector sum (Vector) 
 -}
vecSum :: Vector -> Vector -> Vector
vecSum (x,y,z) (x1,y1,z1) = (x+x1,y+y1,z+z1)

{- -----------------------------------------------------------------
 - vecMagnitude
 - -----------------------------------------------------------------
 - Description: vectorMagnitude takes in a vector (Vector), and 
 - returns a the magnitude of the vector (Double)
 -}
vecMagnitude :: Vector -> Double
vecMagnitude (x,y,z) = sqrt(x^2+y^2+z^2)

{- -----------------------------------------------------------------
 - vecInnerProd
 - -----------------------------------------------------------------
 - Description: vecInnerProd takes in two vectors (Vector) and returns
 - the dot product (Double)
 -}
vecInnerProd :: Vector -> Vector -> Double
vecInnerProd (x,y,z) (x1,y1,z1) = x*x1+y*y1+z*z1

{- -----------------------------------------------------------------
 - vecF
 - -----------------------------------------------------------------
 - Description: vecF takes in a vector (Vector) and a list of
 - vectors ([Vector])
 - Function checks if list is empty, or has one element
 - If none of the above is true calls function v1, and v2
 - Returns two vectors in form (Vector,Vector)
 -}
vecF :: Vector -> [Vector] -> (Vector,Vector)
vecF _ []  = (vecZero, vecZero) --Returns ((0,0,0),(0,0,0)) if list is empty
vecF _ [v] = (v,v) --If list only has a size of 1
vecF v0 vs = (v1 v0 vs, v2 v0 vs) --If none of the conditions above are true, calls recursive function

{- -----------------------------------------------------------------
 - v1 
 - -----------------------------------------------------------------
 - Description: v1 takes in a vector and list of vectors (Vector)([Vector])
 - Calls function v1Aux which determines the lowest (or equal to) difference in distance in 
 - the list with respect to v0's distance
 - Returns output of v1Aux (vector with lowest difference in distance)
 -}
v1 :: Vector -> [Vector] -> Vector
v1 v0 vs = v1Aux v0 (head vs) (tail vs) 0 (0,0,0) True --values: {0, (0,0,0), True} have no real value, they are just place holders, they are used to save values from previous recursive calls
{- -----------------------------------------------------------------
 - v1Aux
 - -----------------------------------------------------------------
 - Description: v1Aux takes the intial v0 (Vector), head of the vector list (Vector),
 - remaining list of vector ([Vector]), prev (a placeholder to hold minimum magnitude 
 - of distance, Double), ans (is the current answer with the smallest distance, Vector), 
 - first (a bool that is only true for the first call)
 - Returns vector with lowest difference in distance
 -}
v1Aux :: Vector -> Vector -> [Vector] -> Double -> Vector-> Bool -> Vector
v1Aux v0 vh vt prev ans first
    |vt == []              = if(prev < difference) then ans else vh --This is the base case, and checks if the list is empty, if so then returns the answer
    |first == True         = v1Aux v0 (head vt) (tail vt) difference vh False --This is a special case, it checks if this is the first recursive call, since the value of prev is 0 (has no relevance)
    |difference <=  prev   = v1Aux v0 (head vt) (tail vt) difference vh False  --This checks if current difference in distance is less than or equal to the previous difference in distance, if so then function calls itself with current (new) difference, and ans is equal to vh (lowest difference vector)
    |difference > prev     = v1Aux v0 (head vt) (tail vt) prev ans False --If the condition above is not true, this line is called with the previous difference in distance, and previous answer (old values)
    where difference       = abs(vecMagnitude ((vecSum vh (neg v0)))) --The variable difference calculates current difference in distance, by calling functions vecMagnitude, vecSum, and neg


{- -----------------------------------------------------------------
 - v2 
 - -----------------------------------------------------------------
 - Description: v2 takes in a vector and list of vectors (Vector)([Vector])
 - Calls function v1Aux which determines the highest (or equal to) difference in distance in 
 - the list with respect to v0's distance
 - Returns output of v2Aux (vector with highest difference in distance)
 -}
v2 :: Vector -> [Vector] -> Vector
v2 v0 vs = v2Aux v0 (head vs) (tail vs) 0 (0,0,0) True --values: {0, (0,0,0), True} have no real value, they are just place holders, they are used to save values from previous recursive calls
 {- -----------------------------------------------------------------
  - v2Aux
  - -----------------------------------------------------------------
  - Description: v2Aux takes the intial v0 (Vector), head of the vector list (Vector),
  - remaining list of vector ([Vector]), prev (a placeholder to hold maximum magnitude 
  - of distance, Double), ans (is the current answer with the largest distance, Vector), 
  - first (a bool that is only true for the first call)
  - Returns vector with highest difference in distance
  -}
v2Aux :: Vector -> Vector -> [Vector] -> Double -> Vector-> Bool -> Vector
v2Aux v0 vh vt prev ans first
    |vt == []              = if(prev > difference) then ans else vh --This is the base case, and checks if the list is empty, if so then returns the answer
    |first == True         = v2Aux v0 (head vt) (tail vt) difference vh False --This is a special case, it checks if this is the first recursive call, since the value of prev is 0 (has no relevance)
    |difference >=  prev   = v2Aux v0 (head vt) (tail vt) difference vh False --This checks if current difference in distance is greater than or equal to the previous difference in distance, if so then function calls itself with current (new) difference, and ans is equal to vh (highest difference vector)
    |difference < prev     = v2Aux v0 (head vt) (tail vt) prev ans False --If the condition above is not true, this line is called with the previous difference in distance, and previous answer (old values)
    where difference       = abs(vecMagnitude ((vecSum vh (neg v0)))) --The variable difference calculates current difference in distance, by calling functions vecMagnitude, vecSum, and neg

{- -----------------------------------------------------------------
  - neg
  - -----------------------------------------------------------------
  - Description: Takes in a vector (Vector), and using vecScalarProd
  - the function return the negation of the vector (Vector)
  - Returns negative vector
  -}
neg :: Vector -> Vector
neg (x,y,z) = vecScalarProd (-1) (x,y,z)


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 1
 - - Input: vecScalarProd 0 (1,2,3)
 - - Expected Output:(0.0,0.0,0.0)
 - - Acutal Output:(0.0,0.0,0.0)
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 2
 - - Input: vecScalarProd 0.5 (-10,5,0)
 - - Expected Output: (-5.0,2.5,0.0)
 - - Acutal Output: (-5.0,2.5,0.0)
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 3
 - - Input: (-100) (sqrt(2),-0.05,100)
 - - Expected Output:(-141.4213562373095,5.0,-10000.0)
 - - Acutal Output: (-141.4213562373095,5.0,-10000.0)
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 1
 - - Input: vecSum (1,2,3) (-1,-2,-3)
 - - Expected Output: (0.0,0.0,0.0)
 - - Acutal Output: (0.0,0.0,0.0)
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 2
 - - Input: vecSum (100,100,35) ((1/3),3**(-3),-43)
 - - Expected Output:(100.33333333333333,100.03703703703704,-8.0)
 - - Acutal Output:(100.33333333333333,100.03703703703704,-8.0)
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecSum
 - - Test Case Number: 3
 - - Input:vecSum (100,100,35) (sqrt(3),-99.99999,-35.001)
 - - Expected Output:(101.73205080756888,1.0000000003174137e-5,-9.999999999976694e-4)
 - - Acutal Output:(101.73205080756888,1.0000000003174137e-5,-9.999999999976694e-4)
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 1
 - - Input: vecMagnitude (0,0,0)
 - - Expected Output: 0.0
 - - Acutal Output:0.0
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 2
 - - Input: vecMagnitude (-100,1/2,2)
 - - Expected Output: 100.02124774266716
 - - Acutal Output: 100.02124774266716
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecMagnitude
 - - Test Case Number: 3
 - - Input:vecMagnitude (sqrt(3),sqrt(9),sqrt(4))
 - - Expected Output: 4.0
 - - Acutal Output: 4.0
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 1
 - - Input: vecInnerProd (1,2,3) (0,0,0)
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 2
 - - Input: vecInnerProd (sqrt(2),-5,0.5) (sqrt(2),-1,2)
 - - Expected Output: 8.0
 - - Acutal Output: 8.0
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecInnerProd
 - - Test Case Number: 3
 - - Input: vecInnerProd ((1/3),45,-234) (2,-9,2.000001)
 - - Expected Output: -872.3335673333333
 - - Acutal Output: -872.3335673333333
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 1
 - - Input: vecF (56,7,6) [(56,6,7),(6,7,6),(7,6,6)]
 - - Expected Output: ((56.0,6.0,7.0),(6.0,7.0,6.0))
 - - Acutal Output: ((56.0,6.0,7.0),(6.0,7.0,6.0))
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 2
 - - Input: vecF (-56,7,6) [(56,6,7),(6,7,6),(7,6,6)]
 - - Expected Output: ((6.0,7.0,6.0),(56.0,6.0,7.0))
 - - Acutal Output: ((6.0,7.0,6.0),(56.0,6.0,7.0))
 - -----------------------------------------------------------------
  -
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 3
 - - Input:vecF (sqrt(2),-54,(1/3)) [(sqrt(2),-45,23),(4,-2394,233),(0.0034,35465,23),(134,-23,0),(1,1,1)]
 - - Expected Output: ((1.4142135623730951,-45.0,23.0),(3.4e-3,35465.0,23.0))
 - - Acutal Output: ((1.4142135623730951,-45.0,23.0),(3.4e-3,35465.0,23.0))
 - -----------------------------------------------------------------
   -
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 4
 - - Input:vecF (0,0,0) [(1,1,1)]
 - - Expected Output: ((1,1,1),(1,1,1))
 - - Acutal Output: ((1,1,1),(1,1,1))
 - -----------------------------------------------------------------
    -
 - -----------------------------------------------------------------
 - - Function: vecF
 - - Test Case Number: 5
 - - Input:vecF (1,1,1) []
 - - Expected Output: ((0,0,0),(0,0,0))
 - - Acutal Output: ((0,0,0),(0,0,0))
 - -----------------------------------------------------------------
 -}

