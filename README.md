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
  primarykeys <- primaryKeys n
  firstnames <- gen n firstname
  lastnames <- gen n surname
  pure $ map makeUser (zip3 primarykeys firstnames lastnames)
  where
    makeUser (pm,fn,sn) =
      insertStatement "user" ["userId","name"] [show pm,name2 fn sn]

```

Produces the output 

``` 
INSERT INTO USER(userId,name)
VALUES (60646,Melvin Craelius);

INSERT INTO USER(userId,name)
VALUES (52669,Anas Shahriari);

INSERT INTO USER(userId,name)
VALUES (49597,Rasmus Jakobsson);
```

