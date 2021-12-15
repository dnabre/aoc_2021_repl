module Main where
import System.Environment
import System.Exit
import Text.Printf
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet

-- Advent of Code 2021
-- Day 14
--  part 1 solution: 2657
--  part 2 solution: 

part_1_test::[Char]
part_1_test = "day14/aoc_14_test_1.txt"
part_2_test::[Char]
part_2_test = "day14/aoc_14_test_2.txt"
part_1_input::[Char]
part_1_input = "day14/aoc_14_part_1.txt"
part_2_input::[Char]
part_2_input = "day14/aoc_14_part_2.txt"

getManyRare::[Char]->((Char,Int),(Char,Int))
getManyRare ss = (head ml, head (reverse ml))
    where
        ml = sortOn snd $ MultiSet.toOccurList ms
        ms = MultiSet.fromList ss

getManyRareMSet s =  map (\(ch,n) ->  (ch, cc n))       $MultiSet.toOccurList ms
    where
        cc = (\n -> (n + 1) `div` 2)
        ol = MultiSet.toOccurList s
        sol = concatMap (\((a,b),n)-> [(a,n),(b,n)]) ol
        ms = MultiSet.fromOccurList sol

part1 template c_map  n = (o_max - o_min)
    where
        exp0 = stepExpand template c_map n
        mr@((_,o_min),(_,o_max)) =  getManyRare exp0


part2 x = undefined
           
getStringVals :: FilePath -> IO [String]
getStringVals path = do 
                        contents <- readFile path
                        return  (lines contents)
string2int::[Char]->Int               
string2int = r
    where 
        r:: [Char] -> Int
        r = read

rowString2list :: [Char] -> [Int]
rowString2list xs = map string2int (filter (\x->x /= "") $ splitOn " " xs)

splitEmptyLine::[[Char]]->([[Char]],[[Char]])
splitEmptyLine ls = splitEmptyLine' ls []
    where
        splitEmptyLine' ("":xs) p =  (reverse p,xs)
        splitEmptyLine' (x:xs) p = splitEmptyLine' xs (x:p)





parseEqs::[Char] -> (Char, Char, Char)
parseEqs ls =  (a,b,t)
    where
        (ps:ff:_) = splitOn " -> " ls
        a = (head ps)
        b = (head (drop 1 ps))
        t = (head ff)

insertPairListMap::[(Char,Char,Char)] -> Map.Map (Char,Char) Char -> Map.Map (Char,Char) Char
insertPairListMap [] m = m
insertPairListMap ((a,b,t):xs) m = insertPairListMap xs (Map.insert (a,b) t m)



expandPairs:: (Char,Char) -> Map.Map (Char,Char) Char -> [Char]
expandPairs (a,b) m = [a,(getM mb)]
    where
        mb = Map.lookup (a,b) m
        getM Nothing = ' '
        getM (Just t) = t





insertChems::[(Char,Char)]->Map.Map (Char, Char) Char -> [Char]
insertChems pair_list c_map = (concatMap (\p->expandPairs p c_map) pair_list) ++ (lastLet pair_list)

lastLet::[(Char,Char)]->[Char]
lastLet pair_list = [ snd $ head (reverse pair_list) ]

makePairList::[Char]->[(Char,Char)]
makePairList ls = makePairList' ls []
    where
        makePairList' [] acc = reverse acc
        makePairList' (x:[]) acc =reverse acc
        makePairList' (x:y:xs) acc = makePairList' (y:xs) ((x,y):acc)


stepExpand::[Char]-> Map.Map (Char,Char) Char -> Int -> [Char]
stepExpand p_list _ 0 = p_list
stepExpand p_list c_map n = stepExpand (insertChems (makePairList p_list) c_map) c_map (n-1)

stepOCExpand::MultiSet.MultiSet (Char,Char) -> Map.Map (Char, Char) Char -> Int -> MultiSet.MultiSet (Char, Char)
stepOCExpand m_set _ 0 = m_set
stepOCExpand m_set c_map n =  stepOCExpand (next_mset ) c_map (n-1)
    where
        ol = MultiSet.toOccurList m_set
        n_ol= concatMap (\o->expandOccurs o c_map) ol
        next_mset = MultiSet.fromOccurList n_ol


expandOccurs :: ((Char,Char),Int) -> Map.Map (Char,Char) Char -> [((Char,Char),Int)]
expandOccurs ((a,b),n) m = [((a,c),n),((c,b),n)]
    where
        getM Nothing = ' '
        getM (Just t) = t
        c = getM (Map.lookup (a,b) m)


expandPair:: (Char, Char) -> Map.Map (Char, Char) Char -> [(Char,Char)]
expandPair (a,b) c_map = [(a,c), (c,b)]
    where
        m_lookup = Map.lookup (a,b) c_map
        getM Nothing = ' '
        getM (Just t) = t
        c = getM m_lookup

makeMSPairs::[(Char,Char)] -> MultiSet.MultiSet (Char,Char)
makeMSPairs ss = MultiSet.fromList ss



listToMSOccurs ls = sort $ MultiSet.toOccurList ( MultiSet.fromList ls)

parseAll vals1 = (template, c_map)
    where
        (template_string,eq_strings) = splitEmptyLine vals1
        template = head template_string
        c_list = map parseEqs eq_strings
        c_map = insertPairListMap c_list Map.empty

main :: IO()
main = do 
            printf "Advent of Code 2021, Day 14:\n"
            vals1 <- getStringVals part_1_input
            printf "    read %d lines of input\n" (length vals1)
            vals2 <- getStringVals part_2_test
            printf "    read %d lines of input\n" (length vals2)

            let (template1, c_map1) = parseAll vals1
         --   let answer1 = part1 template1 c_map1 10
         --   printf "\n   Part 1    Solution: %d \n" answer1

            let (template,c_map) = parseAll vals2
            printf "\n    Template    : %s   \n    %s\n" template (show  $ listToMSOccurs template)
            let step1 = stepExpand template c_map 1
            printf "    After step 1: %s   \n    %s\n" step1 (show . sort $ listToMSOccurs  step1)
            printf "        rare/few: %s \n" (show $ getManyRare step1)
            let step2 = stepExpand template c_map 2 
            printf "    After step 2: %s   \n    %s\n" step2 (show . sort$ listToMSOccurs  step2)
            printf "        rare/few: %s \n" (show $ getManyRare step2)
            let step3 = stepExpand template c_map 3 
            printf "    After step 3: %s   \n    %s\n" step3 (show . sort$ listToMSOccurs  step3)
            printf "        rare/few: %s \n" (show $ getManyRare step3)
            let step4 = stepExpand template c_map 4  
            printf "    After step 4: %s   \n    %s\n\n\n" step4 (show .sort $ listToMSOccurs  step4)
            printf "        rare/few: %s \n" (show $ getManyRare step4)
            print (makePairList template)
            let pairs_mset = makeMSPairs (makePairList template)
            print pairs_mset
            
            let oc_list = MultiSet.toOccurList pairs_mset
            print oc_list
            
            let oc_list2 = concatMap (\o->expandOccurs o c_map) oc_list
            let oc_set2 = MultiSet.fromOccurList oc_list2
            printf "\n-> "
            print oc_set2
            printf "        rare/few: %s \n" (show $ getManyRareMSet oc_set2)
            let oc_list3 = concatMap (\o->expandOccurs o c_map) oc_list2
            let oc_set3 = MultiSet.fromOccurList oc_list3
            printf "\n-> "
            print oc_set3
            printf "        rare/few: %s \n" (show $ getManyRareMSet oc_set3)
            let oc_list4 = concatMap (\o->expandOccurs o c_map) oc_list3
            let oc_set4 = MultiSet.fromOccurList oc_list4
            printf "\n-> "
            print oc_set4
            printf "        rare/few: %s \n" (show $ getManyRareMSet oc_set4)
            let oc_list5 = concatMap (\o->expandOccurs o c_map) oc_list4
            let oc_set5 = MultiSet.fromOccurList oc_list5
            printf "\n-> "
            print oc_set5
            printf "        rare/few: %s \n" (show $ getManyRareMSet oc_set5)
            let test_5 = (stepOCExpand pairs_mset c_map 4)
            print test_5
            printf "        rare/few: %s \n" (show $ getManyRareMSet test_5)
            
            
  

        


--stepExpand::[Char]-> Map.Map (Char,Char) Char -> Int -> [Char]
--stepExpand p_list _ 0 = p_list
--stepExpand p_list c_map n = stepExpand (insertChems (makePairList p_list) c_map) c_map (n-1)

   --         let answer1 = part1 template1 c_map1 10
   --         let answer2 = 0

  --          printf "\n   Part 1    Solution: %d \n" answer1
          --  printf "\n   Part 2    Solution: %d \n" answer2

            print "done"