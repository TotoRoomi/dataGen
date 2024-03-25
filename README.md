Toto Roomi 
Version 0.0
# QuickPop

A DSL to populate a PSQL database with random dummy data. Uses QuickCheck, hence the name quickcheck+populate=quickpop. 

## Dependencies

### Ubuntu
```
sudo apt install stack 
```

## To run 

```
stack run
```

### Try out some functions 

```
stack repl 
```


## Examples 

## General 

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
