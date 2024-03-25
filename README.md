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



## General example 

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

The function uses 'selfRefPairs' and gives half the size of the userIDs list as the maximum number of friends a single person may have. 

### attends
Attends dictates that some number of people can attend an event. 

``` haskell
attending :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
attending uids eids = do
  pairs <- forEachKeyMakePairs' (1,((length uids * 7) `div` 10)) eids uids
  pure $ insert "Attending" ["UserId","EventId"] pairs
```

Here the function 'forEachKeyMakePairs' is used to create up to n random and unique key-pairs between eventIDs and userIDs. Here up to n random users are chosen per event. n is 70% of the userIDs. 

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
'forEachDateMakeDates' solves this issue by generating a date that is later than or equal to the 'seed' date. 'forrEachKeyMakePairs' returns both a list of integers and the pairs, the integers represents how many pairs each key made. This list is used in 'forEachDateMakeDates' to tell it how many dates to generate per 'seed' date. 

