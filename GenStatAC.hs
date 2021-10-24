import System.Random
import Data.List
import Data.Time

import System.TimeIt
-- Haskell code for testing the functions genFIt_1, genFIt_2, genFIO
-- Testing is done using integers instead of strings
-- This is because we use predefined Haskell capabilities to generate a random list of integers

-- Algorithmic considerations:
--  1-The list of integers is sorted
--  2-Therefore the occurrences of an integer appear grouped

--  ******************************************** 
--The variant in which 3 functions are used
--Input: List of sorted integers
--Output: A list of pairs, each pair having as first element an integer and the second element the number of appearances

--
-- Algorithm:
--   1- Take the first item in the list
--   2- For him the number of appearances in the list is determined with the elements of the list except the first
--   3- The appearances of the first element from the initial list are deleted
--   4- Continue with step 1 if the list has not been emptied
-- Observations regarding complexity:
--   For each integer his appearances are explored twice 
--   So if {n1, n2, ..., nk} are the numbers and the related frequencies are {f1, f2, ..., fk}
--   we will have 2 * f1 + 2 * f2 + ... + 2 * fk accesses of the list elements

genFIt_1::[Int]->[(Int,Int)]
genFIt_1 []=[]
genFIt_1 lnint=((head lnint),(contap3 (head lnint) (tail lnint))):(genFIt_1 (delap (head lnint) lnint))


contap3::Int->[Int]->Int
contap3 _ []=1
contap3 cuv (cc:rl) |(cuv==cc)=1+(contap3 cuv rl)
                    |otherwise=1

delap::Int->[Int]->[Int]
delap _ []=[]
delap cuv (cc:rl) |(cuv==cc)=(delap cuv rl)
                    |otherwise=(cc:rl)
                    
genFIt_2::[Int]->[(Int,Int)]
genFIt_2 []=[]
genFIt_2 lnint=((head lnint),fcuv):(genFIt_2 rl)
            where (fcuv,rl)=contap2 (head lnint) lnint


contap2::Int->[Int]->(Int,[Int])
contap2 _ []=(0,[])
contap2 cuv (cc:rl)=if (cuv==cc) then ((1+fi),rli)
                                else (0,(cc:rl))
                   where (fi,rli)=contap2 cuv rl 

genFIO::[Int]->IO [(Int,Int)]

genFIO lnr=do
{
  if (lnr==[]) then return []
               else do
               { 
                 (nrap,rl)<-return (contap2 (head lnr) lnr);
                 lsi<-genFIO rl;
                 return (((head lnr),nrap):lsi)
               }
}
            
-- **************************************                   


-- Generate the list of random integers for testing

randomList :: Int -> StdGen -> [Int]
randomList n = take n.unfoldr (Just . randomR(1,100))


main::IO()
main=do
{
  ltest<-return [1,1,1,2,2,2,2,3]; 
  seed<-newStdGen;
  lista<-return (randomList 2000000 seed);
  listas<-return (sort lista);
  
  putStrLn "|-------------------------------------|";
  putStrLn "|Number of items in the list:2.000.000|";  
  putStrLn "|-------------------------------------|";

  putStrLn "|Function tested: <genFIt_1>...      |"; 
  timeIt $ (return (genFIt_1 listas))>>=(\lstat->putStrLn $ "|"++ (show (last lstat)));
  putStrLn "|------------------------------------|"; 

  putStrLn "Function tested: <genFIt_2>...       |"; 
  timeIt $ (return (genFIt_2 listas))>>=(\ls->putStrLn (show (last ls)));
  putStrLn "|------------------------------------|"; 

  putStrLn "Function tested: <genFIO>...         |"; 
  timeIt $ (genFIO listas)>>=(\lsio->putStrLn (show (last lsio)));
  putStrLn "|------------------------------------|"; 
 
}  

  
  



