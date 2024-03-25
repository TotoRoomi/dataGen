Author:  Toto Roomi 
Version: 0.0
# QuickPop

A DSL to populate a PSQL database with random dummy data. Uses QuickCheck, hence the name quickcheck + populate = quickpop. 

## Dependencies

### Ubuntu
```
sudo apt install stack 
```

## To use 

```
stack run > <filename>.psql
```

### Try out some functions 

``` 
stack repl 
```


## The library 


### PSQSLTYPE 
``` haskell
data PSQLTYPE
  = VARCHAR String
  | DATE (Year, Month, Day) 
  | INTEGER Int
  | TIMESTAMP (Year,Month,Day) (Hour,Min,Sec) Int Bool
  deriving (Eq)

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Min = Int
type Sec = Int
```
This datatype represents a few datatypes used in PSQL. All are self explanitory except TIMESTAMP. The last Int represents the timezone and the Boolean wether to include it or not. As PSQL has several representations of the TIMESTAMP datatype. 
To convert to and from these the following functions are provided: 

``` haskell
psqlVarchar :: String -> PSQLTYPE
unVarchar :: PSQLTYPE -> String
psqlDate :: (Int,Int,Int) -> PSQLTYPE
unDate :: PSQLTYPE -> (Int,Int,Int)
psqlInteger :: Int -> PSQLTYPE
unInteger :: PSQLTYPE -> Int
psqlTimestamp :: (Int,Int,Int) -> (Int,Int,Int) -> Int -> Bool -> PSQLTYPE
```

### Gen PSQLTYPE 
There are many random data generators, varying from name and email to date and text generators. You can read about them in the haddock documentation, pre-compiled in ```doc/haddock/index.html```. However the vital generators are the following:

``` haskell
primaryKeys :: Int -> Gen [PSQLTYPE]
```
This generator creates unique numbers between 11111 and 99999. The numbers were arbitrarily chosen because five successive numbers look nice. A larger database is not expected to be created using this library so it is more than enough. 

``` haskell
make :: Int -> Gen a -> Gen [a]
unique :: (Eq a) => Int -> Gen a -> Gen [a]
```
The idea behind these was that you should be able to read the code as if it's human language "Make one hundred emails", "unique ten names". They are fairly self documenting and work as expected, however a caveat is that unique is very slow. It functions at a O(n^3) speed because it keeps generating and removing duplicates, thus it was capped at up to 1000 iterations. The databases produced by this library are not expected to be large, and to keep the niceness of primaryKeys with five numbers it was done this way. Another way would be to introduce a freshness generator that simply iterates. Thus generating a list of n unique numbers would only take O(n) time. 

### Populator 
While the generators create the random data, the populator creates insert statements in the PSQL format and combines, molds and forms generated data into more useful shapes. 

``` haskell
data InsertStatement
 = IS { schemaName :: String,
        attributes :: [String],
        values :: [PSQLTYPE]
      }
 | Statements [InsertStatement]
```
The InsertStatement datatype represents a postgresQL insert statement. 
To create insert statements the following functions are provided: 

``` haskell
insertStatement :: String -> [String] -> [PSQLTYPE] -> InsertStatement
insert :: String -> [String] -> [[PSQLTYPE]] -> InsertStatement
```
The first creates a single insert statement, however the latter creates many insert statements to a single table denoted by the first String, the name of the table. This is followed by the attribute names and finally a list of data to insert. 
The list is a list of lists of data. If you are inserting to a table with usernames and socialsecurity numbers you would send the following list to the function ```[[usernames],[socialsecurity nrs]]```. The function takes care of the rest. Null values are not supported so they have to be of the same lengths. 

Additionally the Populator provides combiinators that create pairs of keys in different shapes. Thesee are again documented in the haddock.

### Pretty printing 
A pretty printer is provided to actually print the data in the format that postgres accepts, along with several debuging printers.

``` haskell
pretty :: Gen InsertStatement -> IO ()
```
pretty takes either a single statement or several statements and prints them to standard output.


## General example 
Finally some examples! 

``` haskell
user :: [PSQLTYPE] -> Gen InsertStatement
user uids = do
  let n = length uids
  fns <- firstnames n
  lns <- lastnames n
  let names = zipWith name2 fns lns
  pure $ insert "user" ["userId","name"] [uids,names]
```

Produces 

``` 
INSERT INTO USER(userId,name)
VALUES (60646,Melvin Craelius);

INSERT INTO USER(userId,name)
VALUES (52669,Anas Shahriari);

INSERT INTO USER(userId,name)
VALUES (49597,Rasmus Jakobsson);
```

If you would like to try some functions out in the terminal, open a terminal window in the project folder and type: 

``` 
$ stack repl
```

GHCI starts up and loads the files, then try: 

```
ghci> generate (date "2024")
```

generate takes a "Gen a" and produces an "IO a". If you try this with a "Gen [a]" you'll see that the output is quite unappealing. Instead try the pretty printing function "pretty" that takes a "Gen InsertStatement" and prints it in a readible way. 

```
pretty $ user 10
```

## App/Example.hs 
![ER diagram and schemas](doc/ERdiag.png)

The Example file shows how to create insert statements for the database above. We will look at the following interesting samples: friend, attends, textPost and likes. These exemplify key features of the library. 

### friend 
Friend is a self referential table that is non-reflexive. Two people cannot be friends twice in the list as that is a waste of storage space. 

``` haskell
friend :: [PSQLTYPE] -> Gen InsertStatement
friend uids = do
  l <- selfRefPairs' ((length uids) `div` 2) uids
  pure $ insert "Friend" ["UserId","FriendID"] l
```

The function uses ```selfRefPairs``` and gives half the size of the userIDs list as the maximum number of friends a single person may have. 



### attends
Attends dictates that some number of people can attend an event. 

``` haskell
attending :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
attending uids eids = do
  pairs <- forEachKeyMakePairs' (1,((length uids * 7) `div` 10)) eids uids
  pure $ insert "Attending" ["UserId","EventId"] pairs
```

Here the function ```forEachKeyMakePairs``` is used to create up to n random and unique key-pairs between eventIDs and userIDs. Here up to n random users are chosen per event. n is 70% of the userIDs. 

### textPost 
TextPost is interesting as it gives a use case for generating random text. 
``` haskell
textPost :: [PSQLTYPE] -> Gen InsertStatement
textPost pids = do
  texts <- make (length pids) goodText
  pure $ insert "TextPost" ["PostID","Text"] [pids,texts]
```
Make is used to generate a random string of text for each postID. 

### likes 
Likes poses a challenge as would be wonky if posts were liked before they were created. 

``` haskell
likes :: [PSQLTYPE] -> [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
likes uids pids ds = do
  (ns, pairLists) <- forEachKeyMakePairs (0,length uids) pids uids
  d <- forEachDateMakeDates ds ns
  let dates = concat d
  let uids' = head . tail $ pairLists
  let pids' = head pairLists
  pure $ insert "Likes" ["UserID", "PostID", "Date"] [uids',pids',dates]

```

```forEachDateMakeDates``` solves this issue by generating a date that is later than or equal to the 'seed' date. ```forEachKeyMakePairs``` returns both a list of integers and the pairs, the integers represents how many pairs each key made. This list is used in ```forEachDateMakeDates``` to tell it how many dates to generate per 'seed' date. 


## Challenges 
### Text 
There were several challenges in this project. One of which was generating actual grammatically correct text. 
One of the best ways to do this is through markov chains, where you have a sort of map of words or phrases and likey successor words or phrases. 
```
("My",["name is","phone number is", ...])
... 
```
However this would require a large such database of markov chains and with the little research I did I couldn't find a free one. 
Another strategy is to use an Ai API where you query for  some type of text and it responds with that text. However such API's generally cost money. 
The strategy I settled on was generating syntactically correct sentances using basic grammatical rules. 
```<noun phrase> <predicate> <preposition> <nounphrase>``` 
And randomly generate the different words and phrases through a large list of such words. 
This worked fairly well, though some sentances were a bit strange "A green meadow argues soothing music". 
I continued with this and made several varieties and combinations of sentances. The result is a function that randomly chooses some text generator or combination of generators and spits out interesting things. 

"Gentle rain designs a twisting road. That makes me feel grouchy"

"I'm feeling skeptical about the magnificent life drawing event."

"The starry sky fights a busy street"

I designed the event title generator in a similar way. 

```
<Adjective><Event>
```

Which resulted in amusing titles. 

"Volunteer Karaoke"

"Impatient pub"

"Shy Charity fundraiser"

As someone who is active in the board of a student union, this is also a useful brainstorming tool for event planning. 

### Dates 
Dates were also a challenge. At first I created the following function to generate random dates based on a year. 

``` haskell
date :: Int -> Gen PSQLTYPE
date year = do
  month <- chooseInt (1,12)
  day <- dayGen month
  pure $ psqlDate (year, month, day)
  where
    dayGen m | m == 2 = chooseInt (1,28)
             | m == 4 || m == 6 || m == 9 || m == 11
             = chooseInt (1,30)
             | otherwise = chooseInt (1,31)
```

However it quickly became outdated as I needed a way to generate dates between two dates. I tried several ways of doing this but all failed. Finally, I found the library ```Data.Time.Calendar``` that supported the enumiration class instance. This solved the problem almost entirely as it was now just about generating a list from a date to a date and choosing a random element. 

This issue deepened however when I was trying to create the inserts for the  ```likes``` schema. At first I just used the previous function ```dateBetween fromDate toDate``` and defined that all likes occur within a span of time and all posts occur withing a span before that. This works but is very ugly and not believable. So I wrote the function

``` haskell

forEachDateMakeDates :: [PSQLTYPE] -> [Int] -> Gen [[PSQLTYPE]]
```

It creates a series of dates that occur later than the original seed date for each seed date in the first list. The second input is how many likes each post has, thus each seed date gets the appropriate number of like dates. 

### Pairs 
Creating pairs by choosing n number of keys for each seed key proved challenging. Not only that but doing it in several ways
- choosing up to n unique keys for each key from itself and avoiding choosing yourself. 
- choosing up to n unique keys from a different list for each key in a list. 

I solved the first and second with the following higher order function

``` haskell
pairFactory :: (Int,Int) -> [PSQLTYPE] -> [PSQLTYPE]
               -> ([PSQLTYPE] -> (PSQLTYPE,Int,Int)
                   -> Gen (Int,[(PSQLTYPE,PSQLTYPE)])
                  )
               -> Gen ([Int],[[PSQLTYPE]])
pairFactory fromTo k1 k2 f = do
  ns <- make (length k1) $ chooseInt fromTo
  let k1n = zip3 k1 ns [1,2..] 
  nrAndPairs <- mapM (f k2) k1n 
  let (nrs,pairs) = unzip nrAndPairs
  let (l1,l2) = unzip . concat $ pairs
  pure (nrs,[l1,l2])

```
It takes an lower and upper bound of choises, two lists a k1 and k2, a function that defined how the choises are made from k2 for each k1_i and it spits out a tuple of how many choises were made for each k1_i and a list of two lists [[k1_k],[k2_k]]. At first these were a tuples [(k1_k,k2_k)] but I changed it to a list for reasons found in the next chapter. 

With this you would only need to provide the choice-function and it would do the rest. It's flexible enough so you can choose from the same list i.e. k1 chooses n unique elements from k1 or k1 chooses n unique elements from k2. 

``` haskell
-- First problem 
selfRefPairs :: Int -> [PSQLTYPE] -> Gen ([Int],[[PSQLTYPE]])
selfRefPairs max l = pairFactory (0,max) l l f
    where
    f l (p, n, index)
      | drop index l == [] = pure (0,[])
      | otherwise = do
         let l' = drop index l
         ps <- make n $ do
           a <- elements l'
           pure (p,a)
         let ps' = nub ps
         pure (length ps', ps')

-- Second problem 
forEachKeyMakePairs :: (Int,Int) -> [PSQLTYPE] -> [PSQLTYPE] -> Gen ([Int],[[PSQLTYPE]])
forEachKeyMakePairs fromTo k1 k2 = pairFactory fromTo k1 k2 f
  where
    f l (p, n, index) = do
      ps <- make n $ (do
           a <- elements l
           pure (p,a))
      let ps' = nub ps
      pure (length ps',ps')
```
Two birds with one stone! 

### Insert 
At first I made functions for each schema to create insert statements. This was using zipped tuples of data and looked like this:

``` haskell
user :: [PSQLTYPE] -> Gen InsertStatement
user uids = do
  let n = length uids
  fns <- firstnames n
  lns <- lastnames n
  pure $ statements $ map makeUser (zip3 uids fns lns)
  where
    makeUser (pm,fn,sn) =
      insertStatement "user" ["userId","name"] [pm,name2 fn sn]
```

There was no general way of doing it or a function that could decide if zip, zip3, zip4 etc should be used. Schemas could also have many more attributes than the zip functions could accomodate. I needed a better way of doing it where instead of mapping on a zipped list I could just send in an arbitrary number of attribute value lists and the function takes care of the rest. 

Solution: 

``` haskell
user :: [PSQLTYPE] -> Gen InsertStatement
user uids = do
  let n = length uids
  fns <- firstnames n
  lns <- lastnames n
  let names = zipWith name2 fns lns
  pure $ insert "user" ["userId","name"] [uids,names]
```

Indeed the solution looks much more elegant and is easier to read, but how does it work? 

``` haskell

insert :: String -> [String] -> [[PSQLTYPE]] -> InsertStatement
insert s as pss = statements $ map (insertStatement s as) (listZip pss)
```

Insert takes a list of lists and produces insert statements, the magic happens in listZip. 

``` haskell
listZip :: [[PSQLTYPE]] -> [[PSQLTYPE]]
listZip l = go l [] where
  go ([]:_) acc = acc
  go l acc = go (map (drop 1) l) (map head l : acc)
```

This function takes the head of each list and puts them together sort of like zip does: 

``` 
ghci> listZip [[1,2,3,4],[5,6,7,8]]
[[4,8],[3,7],[2,6],[1,5]]
```
Thus when you map on the resulting list you have an element of each attribute. 
The downside however is that every list has to be of the same length.

This solved the cumbersome syntax and allowed functions to be much cleaner and more elegant. 
