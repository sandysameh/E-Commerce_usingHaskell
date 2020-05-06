import System.Random
import System.IO.Unsafe

users=["user1","user2","user3","user4"]
items = ["item1" ,"item2" ,"item3" ,"item4" ,"item5","item6" ]
purchasesHistory = [("user1" , [[ "item1" , "item2" , "item3" ] , [ "item1" , "item2" , "item4" ] ] ) ,("user2" , [ [ "item2" , "item5" ] , [ "item4" , "item5" ] ] ),("user3" , [[ "item3" , "item2" ]]) ,("user4" , [])]

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0,x)))
 
createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList []=[]
createEmptyFreqList (x:xs)= (x,[]):createEmptyFreqList xs

--counts no of elment in a list
count::Eq a=>[a]->a->Int
count [] _=0
count (x:xs) item = if item==x then 1+ count xs item
else count xs item


--removes the head and removes the head if repeated in another place
remove1 ::Eq a=>[a]->[a]
remove1 [] =[]
remove1 (x:xs)  = removehelp (x:xs) x

--help the removal by taking to inputs
removehelp:: Eq a=>[a]->a->[a]
removehelp [] _=[]
removehelp (x:xs) b = if b==x then removehelp xs b 
else x:removehelp xs b  

--concatinates list of lists into one list 

makeOne ::[[a]]->[a]
makeOne []=[]
makeOne (x:xs)= x ++ makeOne xs 

-- helps us get a list for all the list that containes the item to help in counting 

figureOut:: Eq a=>[[a]]->a->[a]
figureOut [] _ =[]
figureOut (x:xs) h = if elem h x then filter (/= h) x ++ figureOut xs h 
else figureOut xs h 


--take item 1 and puts el list of el hagat that i bought it with 
--nowTurn::[[a]]->a->()
nowTurn listoflists x = (x,( finalstep ( figureOut listoflists x ) (figureOut listoflists x ))) 




--workOnit is Responsible for placing item 1[with all commen and counts ] & also Responsible for item 2 and so one

workOnit _ []=[]
workOnit listoflists (x:xs)= if elem x (makeOne listoflists) then nowTurn listoflists x : workOnit listoflists xs
else (x,[]):workOnit listoflists xs

--The helper (I NEED HELP IN CZ ITS NOT DYMANIC IT TAKES ITEM FROM DATA AND BUILDES UP MY FUNCTION DEPENDING ON IT )
helper listoflists = workOnit listoflists items


--calculates the items and puts them in a list of tuples (item 2 ,2 ...)
finalstep::Eq a=>[a]->[a]->[(a,Int)]
finalstep _ []=[]
finalstep list (x:xs)=(x,(count list x)): finalstep list (remove1 (x:xs)) 





getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats []=[]
getAllUsersStats ((a,list):xs) =(a ,helper list):getAllUsersStats xs 
----------------------------
--[("gg",5),("gg",8),("l",4)] it sums up value if same and returns a list of tuples 
--Output will be [("gg",13),("l",4)] 


test tup = sumAll
  where
    collect ys [] = ys
    collect ys (x:xs) =
        if (fst x) `notElem` ys
        then collect (fst x : ys) xs
        else collect ys xs
    collectAllNames = collect [] tup

    sumOne [] n x = (x, n)
    sumOne (y:ys) n x =
        if fst y == x
        then sumOne ys (n + snd y) x
        else sumOne ys n x

    sumAll = map (sumOne tup 0) collectAllNames
-----
--freqListItems
freqListItems user =test (shofFreq user (getAllUsersStats purchasesHistory))


shofFreq _ []=[]
shofFreq user ((u,list):xs) = if user==u then khodElList list
else shofFreq user xs 

--it disregardes the head of tuple and makes the list of second part of tuple one 
khodElList [] =[]
khodElList ((item,list):xs)= list ++ khodElList xs

-------

--freqListCart

--get the cart of the user
ya5odGambElUser user [] = []
ya5odGambElUser user ((u,l):xs)= if user == u then l else ya5odGambElUser user xs

conc [] l = []
conc (x:xs) list = helpconc x list ++ conc xs list

helpconc x [] = []
helpconc x ((item,list):ys) = if x==item then list  else helpconc x ys
freqListCart user []=[]
freqListCart user (x:xs) = test (conc (x:xs) (ya5odGambElUser user (getAllUsersStats purchasesHistory)))
----

purchasesIntersection _ []=[]
purchasesIntersection firstCart ((b,list2):r2)= p2 firstCart list2 : purchasesIntersection firstCart r2
p2 [] _=[]
p2 _ []=[]
p2 ((a,list):r) (c:cs)=if snd c==[] || list==[] then p2 r cs else (a,test (list++snd c)):p2 r cs
-----


freqListCartAndItems user list = test  ((freqListItems user) ++ (freqListCart user list))


---

listofFreq (_,0)=[]
listofFreq (item,x) = item:listofFreq (item,x-1)

--it makes [item1,4]-->[item1,item1,item1,item1]

listFreqKebera []=[]
listFreqKebera (x:xs)=listofFreq x++listFreqKebera xs 


recommendEmptyCart :: String -> String

recommendEmptyCart user =if (listFreqKebera (freqListItems user))==[] then "" else listFreqKebera (freqListItems user) !! randomZeroToX (length (listFreqKebera (freqListItems user))-1) 


recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user list= if (listFreqKebera (freqListItems user))==[] then "" else (listFreqKebera (freqListCartAndItems user list))!! (randomZeroToX (length (listFreqKebera (freqListCartAndItems user list))-1))

--recommendBasedOnUsers (USES freqListCart) # random (length -1) take caare cuz u want elements from 0->4 bas length =5 
recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user = if (listFreqKebera (freqListUsers user))==[] then "" else listFreqKebera (freqListUsers user) !! randomZeroToX (length (listFreqKebera (freqListUsers user ))-1)







-- FreqListUsers
freqListUsers:: String -> [(String, Int)]
freqListUsers user = test (khodElList (makeOne((purchasesIntersection (ya5odGambElUser user (getAllUsersStats purchasesHistory)) (removetuple user (getAllUsersStats purchasesHistory))))))

removetuple _ [] = []
removetuple user ((u,list):xs) | user == u  = xs
                    | otherwise = (u,list) : removetuple user xs
					
recommend :: String -> [String] -> String
recommend user list=if [recommendBasedOnItemsInCart user list]++[recommendBasedOnUsers user]==["",""] then items!!(randomZeroToX (length (items)-1)) else ([recommendBasedOnItemsInCart user list]++[recommendBasedOnUsers user])!!(randomZeroToX (length ([recommendBasedOnItemsInCart user list]++[recommendBasedOnUsers user])-1))


test1 []=[]
test1 listoftuple =testhelper (listFreqKebera listoftuple) (listFreqKebera listoftuple)

testhelper _ []=[]
testhelper list (x:xs) =(x,(count list x)):testhelper  (remove1 list) (remove1 list)




