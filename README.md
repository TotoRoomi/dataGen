Toto Roomi 
Version 0.0
# dataGen

A DSL to generate random dummy data for your psql database

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

``` haskell
insertUsers :: Int -> Gen [InsertStatement]
insertUsers n = do
  primarykeys <- primaryKeys n -- generate a list of n random unique primaryKeys 
  firstnames <- gen n firstname  -- generate n random firstnames 
  lastnames <- gen n surname  -- generate n random lastnames 
  pure $ map makeUser (zip3 primarykeys firstnames lastnames)  -- for each tuple (key, firtname, lastname) make a user 
  where
    makeUser (pm,fn,sn) =
      insertStatement "user" ["userId","name"] [show pm,name2 fn sn]  -- schemaName, list of attribute names, the values for each attribute 

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

generate takes a "Gen a" and produces an "IO a". If you try this with a "Gen [a]" you'll see that the output is quite unappealing. Instead try the printing functions "pretty" and "prettyList". (These will be refactored in the future). 

```
pretty $ date "2024"
prettyList $ insertUsers 10 
```
