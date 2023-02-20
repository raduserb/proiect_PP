
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1
process_line :: Row -> Row
process_line l =  [(head l) , (printf "%.2f") ((foldl (+) 0 (map (read ::String -> Float) (tail l))) / 8)]

compute_average_steps :: Table -> Table
compute_average_steps m = ["Name","Average Number of Steps"] : map process_line (tail m)


-- Task 2

-- Number of people who have achieved their goal:
get_total_steps_person :: Row -> Int
get_total_steps_person l = (foldl (+) 0 (map (read ::String -> Int) (tail l)))

get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr (\x acc -> if x >= 1000 then (acc+1) else acc) 0 (map get_total_steps_person (tail m))


-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m =  ((fromIntegral (get_passed_people_num m)) / (fromIntegral ((length m) -1)))

-- Average number of daily steps
get_total_steps :: Table -> Int
get_total_steps m = foldr (+) 0 (map get_total_steps_person (tail m))

get_steps_avg :: Table -> Float
get_steps_avg m = (fromIntegral ((get_total_steps m)) / (fromIntegral (length m) - 1)) 


-- Task 3

sum_columns :: Table -> [Float]
sum_columns [r] = (tail (map (read ::String -> Float) r))
sum_columns (x:xs) = zipWith (+) (tail (map (read ::String -> Float) x)) (sum_columns xs)

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m =[ ["H10","H11","H12","H13","H14","H15","H16","H17"] , map (printf "%.2f") (map (/ (fromIntegral ((length m)-1))) (sum_columns (tail m))) ]


-- Task 4

get_very_active_minutes_list :: Table -> [Int]
get_very_active_minutes_list m = map (read ::String -> Int) (map (!!3) (tail m))

get_fairly_active_minutes_list :: Table -> [Int]
get_fairly_active_minutes_list m = map (read ::String -> Int) (map (!!4) (tail m))

get_lightly_active_minutes_list :: Table -> [Int]
get_lightly_active_minutes_list m = map (read ::String -> Int) (map (!!5) (tail m))

get_active_minutes :: [Int] -> [Int]
get_active_minutes l = [f1 l,f2 l,f3 l]
  where 
    f1 :: [Int] -> Int
    f1 l = foldr (\x acc -> if x<50 then acc+1 else acc) 0 l
    f2 :: [Int] -> Int
    f2 l = foldr (\x acc -> if x>=50 && x<100 then acc+1 else acc) 0 l
    f3 :: [Int] -> Int
    f3 l = foldr (\x acc -> if x>=100 then acc+1 else acc) 0 l

transform_to_row :: [Int] -> Row
transform_to_row l = map (show) l

return_very_active_minutes :: Table -> Row
return_very_active_minutes m = "VeryActiveMinutes":(transform_to_row (get_active_minutes (get_very_active_minutes_list m)))

return_fairly_active_minutes :: Table -> Row
return_fairly_active_minutes m = "FairlyActiveMinutes":(transform_to_row (get_active_minutes (get_fairly_active_minutes_list m)))

return_lightly_active_minutes :: Table -> Row
return_lightly_active_minutes m = "LightlyActiveMinutes":(transform_to_row (get_active_minutes (get_lightly_active_minutes_list m)))

get_activ_summary :: Table -> Table
get_activ_summary m = [["column","range1","range2","range3"],return_very_active_minutes m,return_fairly_active_minutes m, return_lightly_active_minutes m]


-- Task 5


get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"]:(map (take 2) (sortBy (\e1 e2-> if (compare (e1 !! 1) (e2 !! 1) == EQ) then compare (e1 !! 0) (e2 !! 0) else compare (e1 !! 1) (e2 !! 1)) (tail m)))


-- Task 6

process_line_first_4h :: Row -> Row
process_line_first_4h l =  [(head l) , (printf "%.2f") ((foldl (+) 0 (map (read ::String -> Float) (take 4 (tail l)))) / 4)]

process_line_last_4h :: Row -> Row
process_line_last_4h l =  [ (printf "%.2f") ((foldl (+) 0 (map (read ::String -> Float) (drop 4 (tail l)))) / 4)]

compute_difference :: Row -> Row
compute_difference m = [(printf "%.2f") (abs (((map (read ::String -> Float) (tail m)) !! 0) - ((map (read ::String -> Float) (tail m)) !! 1)))]

compute_average_steps_4h :: Table -> Table
compute_average_steps_4h m = zipWith (++) (map process_line_first_4h (tail m)) (map process_line_last_4h (tail m))

compute_average_steps_4h_difference :: Table -> Table
compute_average_steps_4h_difference m = zipWith (++) (compute_average_steps_4h m) (map compute_difference (compute_average_steps_4h m))

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"]:(sortBy (\e1 e2-> if (compare (e1 !! 3) (e2 !! 3) == EQ) then compare (e1 !! 0) (e2 !! 0) else compare (e1 !! 3) (e2 !! 3)) (compute_average_steps_4h_difference m))


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m

-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f m)


get_sleep_total :: Row -> Row
get_sleep_total r = [ r !! 0 , (printf "%.2f") (foldl (+) 0 (map (read ::String -> Float) (tail r))) ]


{-
    TASK SET 2
-}

-- Task 1

find_column :: ColumnName -> Table -> Int
find_column s t = find s (head t) 0
    where 
        find s [] acc = -1
        find s t acc = if s == (head t) then acc else find s (drop 1 t) (acc + 1)

transform_to_int :: String -> Int
transform_to_int s = (read :: String -> Int) s


--sorteaza crescator in fct de stringuri
myCompareStrings :: Row -> Row -> Ordering  
myCompareStrings r1 r2
     | (r1 !! 0) > (r2 !! 0)  = GT  
     | (r2 !! 0) == (r1 !! 0)    = EQ  
     | otherwise = LT


--sorteaza crescator in fct de inturi obt din stringuri
myCompareInt :: Row -> Row -> Int -> Ordering  
myCompareInt r1 r2 i
     | transform_to_int (r1 !! i) > transform_to_int (r2 !! i)  = GT  
     | transform_to_int (r2 !! i) == transform_to_int (r1 !! i) = myCompareStrings r1 r2
     | otherwise = LT


tsort :: ColumnName -> Table -> Table
tsort column (x:xs) = x : (sortBy (\e1 e2 -> myCompareInt e1 e2 (find_column column (x:xs)) ) xs)

-- Task 2

vunion :: Table -> Table -> Table
vunion t1 t2 = if (head t1) == (head t2) then t1 ++ (tail t2) else t1

-- Task 3



zipWithPadding :: Table -> Table -> Int -> Int -> Table
zipWithPadding [] [] l1 l2 = []
zipWithPadding (x:xs) [] l1 l2= (x ++ (take l2 (repeat ""))) : (zipWithPadding xs [] l1 l2)
zipWithPadding [] (x:xs) l1 l2= ((take l1 (repeat "")) ++ x) : (zipWithPadding [] xs l1 l2)
zipWithPadding (x:xs) (y:ys) l1 l2= (x ++ y) : (zipWithPadding xs ys l1 l2)

hunion :: Table -> Table -> Table
hunion t1 t2 = zipWithPadding t1 t2 (length (head t1)) (length (head t2))

-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = [[]]

-- Task 5

cartesian_line :: (Row -> Row -> Row) -> Row -> Table -> Table
cartesian_line f l [] = []
cartesian_line f l (x:xs) = (f l x) : cartesian_line f l xs 

cartesian_no_name :: (Row -> Row -> Row) -> Table -> Table -> Table
cartesian_no_name f [] t2 = []
cartesian_no_name f (x:xs) t2 = (cartesian_line f x t2) ++ (cartesian_no_name f xs t2)


cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : (cartesian_no_name new_row_function (tail t1) (tail t2))

-- Task 6

projection_simple :: Int -> Table -> Table
projection_simple i [] = []
projection_simple i (x:xs) = [(x !! i)] : (projection_simple i xs)

projection :: [ColumnName] -> Table -> Table
projection [] t = [[]]
projection (s:xs) t = if ((find_column s t) /= -1) then hunion (projection_simple (find_column s t) t)  (projection xs t) else []

-- Task 7

filterTable_simple :: (Value -> Bool) -> Int -> Table -> Table
filterTable_simple f i [] = []
filterTable_simple f i (t:ts) = if f (t !! i) then (t:(filterTable_simple f i ts)) else filterTable_simple f i ts

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = ((head t):(filterTable_simple condition (find_column key_column t) (tail t)))

