-- |  Generates usable data for insert statements
module Generator
  (PSQLTYPE
  -- * PSQLTYPE constructors
  ,showPSQLTYPE
  ,psqlVarchar
  ,unVarchar
  ,psqlDate
  ,unDate
  ,psqlInteger
  ,unInteger
  ,psqlTimestamp
  -- * Attribute gen combinators
  ,make
  ,unique
  -- * Attribute generators
  ,psqlName
  ,firstnames
  ,lastnames
  ,primaryKeys
  ,name
  ,name2
  ,email
  ,email2
  ,date
  ,dateBetween
  ,timestampBetween
  ,eventTimestampBetween
  ,eventTitle
  ,url
  ,tagList
  ,place
  ,imageFilter
  -- * Text generators
  ,imfeeling
  ,makesmefeel
  ,simpleSentance
  ,simpleSentance2
  ,feelabout
  ,tweet
  ,sentanceAnd
  ,text
  ,goodText
  )
where


import Data
import Test.QuickCheck
import Data.List(intercalate, nub, nubBy)
import Data.Char(toUpper,toLower)
import Control.Monad(replicateM)
import Data.Time.Calendar


--------------------------------------------------------------------------------
-- * PSQL types
--------------------------------------------------------------------------------
-- | Describes a few common types used in PSQL
data PSQLTYPE
  = VARCHAR String
  | DATE (Int, Int, Int) -- year, month, day
  | INTEGER Int
  | TIMESTAMP (Int,Int,Int) (Int,Int,Int) Int Bool
    -- (year,month,day) (hour,minute,second) timezone includeTimeZone
  deriving (Eq)

instance Show PSQLTYPE where
  show = showPSQLTYPE

-- | Used in the PSQLTYPE instance of Show to print
showPSQLTYPE :: PSQLTYPE -> String
showPSQLTYPE t
  = case t of
      VARCHAR s -> "\""++s++"\""
      DATE (y,m,d) -> "\""++showDate y m d++"\""
      INTEGER i -> show i
      TIMESTAMP (y,m,d) (h,min,s) t b -> "\""++showDate y m d ++ " "
                                         ++ showTime h min s ++ showTimezone t b++"\""
    where
      showDate y m d = show y ++ "-"++showDD m++"-"++showDD d
      showTime h m s = showDD h ++ ":" ++ showDD m ++":"++ showDD s
      showTimezone t b | b && (t < 10 && t > (0))  = "+0"++show t
                       | b && (t > (-10) && t < 0)  = "-0"++show (abs t)
                       | b && (t > 9)  = "+"++show t
                       | b = show t
                       | otherwise = ""
      showDD i | i < 10 = "0" ++ show i
               | otherwise = show i


--------------------------------------------------------------------------------
-- * PSQL constructors
--------------------------------------------------------------------------------
-- | Convert string to PSQLTYPE of VARCHAR String
psqlVarchar :: String -> PSQLTYPE
psqlVarchar s = VARCHAR s

-- | Convert VARCHAR to String
unVarchar :: PSQLTYPE -> String
unVarchar s = case s of
  VARCHAR a -> a

-- | Convert a tuple of (year,month,day) to PSQLTYPE Date
psqlDate :: (Int,Int,Int) -> PSQLTYPE
psqlDate d = DATE d

-- | Convert a PSQLTYPE Date to a tuple (year,month,day)
unDate :: PSQLTYPE -> (Int,Int,Int)
unDate p = case p of
  DATE d -> d

-- | Convert Int to PSQLTYPE INTEGER
psqlInteger :: Int -> PSQLTYPE
psqlInteger n = INTEGER n

-- | Convert PSQLTYPE INTEGER to Int
unInteger :: PSQLTYPE -> Int
unInteger p = case p of
  INTEGER i -> i

-- | Convert tuples (year,month,day) (hour,minute,second) to PSQLTYPE TIMESTAMP
psqlTimestamp :: (Int,Int,Int) -> (Int,Int,Int) -> Int -> Bool -> PSQLTYPE
psqlTimestamp ymd hms t b = TIMESTAMP ymd hms t b

--------------------------------------------------------------------------------
-- * Process PSQLTYPE Data
--------------------------------------------------------------------------------

-- Not exported helper
genPSQLTYPEList :: Int -> (a -> PSQLTYPE) -> [a] -> Gen [PSQLTYPE]
genPSQLTYPEList n f l = make n $ elements $ map f l

-- | Generate a list of firstnames
firstnames :: Int -> Gen [PSQLTYPE]
firstnames n = genPSQLTYPEList n psqlVarchar firstname

-- | Generate a list of lastnames
lastnames :: Int -> Gen [PSQLTYPE]
lastnames n =  genPSQLTYPEList n psqlVarchar surname

-- | Generate a full name based on a first and last name in PSQLTYPE Varchar
psqlName :: PSQLTYPE -> PSQLTYPE -> PSQLTYPE
psqlName (VARCHAR first) (VARCHAR last) = VARCHAR (first ++ " " ++ last)


--------------------------------------------------------------------------------
-- * Attribute generators and helpers
--------------------------------------------------------------------------------
-- | Make an n long list of randomly generated items
make :: Int -> Gen a -> Gen [a]
make = replicateM

-- | Create a list of unique generated items of length n
unique :: (Eq a) => Int -> Gen a -> Gen [a]
unique i g = go i 0 g (pure [])
  where
    go i n g gl = do
      l <- gl
      let l' = nub l
      let len = length l'
      if len == i || n == 1000 then pure l'
        else go i (n+1) g (fill (i - len) g l')
    fill k g la = do
      lb <- make k g
      pure $ la ++ lb

-- | Produces a list of unique primary keys at the specified length
primaryKeys :: Int -> Gen [PSQLTYPE]
primaryKeys i = unique i g
  where g = do
          i <- chooseInt (11111,99999)
          pure $ psqlInteger i

-- | generates a random full name
name :: Gen PSQLTYPE
name = do
  fn <- elements firstname
  ln <- elements surname
  pure . psqlVarchar $ fn ++ "" ++ ln

-- | Generates a full name using the given first and lastname
name2 :: PSQLTYPE -> PSQLTYPE -> PSQLTYPE
name2 (VARCHAR fn) (VARCHAR sn) = VARCHAR $  fn ++ " " ++ sn

-- | Generates a random email
email :: Gen PSQLTYPE
email = do
  fn <- elements $ firstname
  sn <- elements $ surname
  n <- num
  k <- num
  pure . psqlVarchar $ (take n fn)++(take k sn)++emaildomain
  where
    num = chooseInt (1,5)

-- | Generates an email with the specified first and lastname
email2 :: PSQLTYPE -> PSQLTYPE -> Gen PSQLTYPE
email2 (VARCHAR fn) (VARCHAR sn) = do
  n <- num
  k <- num
  pure . psqlVarchar $ (take n fn)++(take k sn)++emaildomain
  where
    num = chooseInt (1,5)

-- | Creates a date with the specified year
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

-- | Generate a date between two dates of the form (year,month,day)
dateBetween :: (Int,Int,Int) -> (Int,Int,Int) -> Gen PSQLTYPE
dateBetween (fy,fm,fd) (ty,tm,td) = do
  let from = fromGregorian (toInteger fy) fm fd
  let to = fromGregorian (toInteger ty) tm td
  date <- elements $ enumFromTo from to
  let (y,m,d) = toGregorian date
  pure . psqlDate $ (fromInteger y,  m,  d)

-- | Provides a random timestamp between two dates, the time is random though
timestampBetween :: (Int,Int,Int) -> (Int,Int,Int) -> Gen PSQLTYPE
timestampBetween from to = do
  hour <- elements [0,1..23]
  min <- elements [0,1..59]
  dymd <- dateBetween from to
  case dymd of
    DATE ymd -> pure $ psqlTimestamp ymd (hour,min,0) 0 False

-- | Generates two timestamps on the same day but hours between each other
eventTimestampBetween :: (Int,Int,Int) -> (Int,Int,Int) -> Gen (PSQLTYPE,PSQLTYPE)
eventTimestampBetween from to = do
  randomIndex <- chooseInt (0,22)
  let hour = [0,1..23] !! randomIndex -- choose a random index
  hour2 <- elements (drop (randomIndex+1) [0,1..23]) -- choose a random hour from the remaining hours
  minute <- elements [0,15,30,45]
  minute2 <- elements [0,15,30,45]
  dymd <- dateBetween from to
  case dymd of
    DATE ymd -> pure $ ( psqlTimestamp ymd (hour,minute,0) 0 False
                       , psqlTimestamp ymd (hour2,minute2,0) 0 False
                       )

-- | Generate a random event title
eventTitle :: Gen PSQLTYPE
eventTitle = do
  adj <- elements adjective
  event <- elements activity
  let s = adj ++ " " ++ event
  pure . psqlVarchar $ s

-- | Generate a random URL of the form "http://kthsocial.com/S/i*i"
--   where S is the string passed and i is the PSQLTYPE Integer passed
url :: String -> PSQLTYPE ->Gen PSQLTYPE
url t (INTEGER i) = pure . psqlVarchar $ "http://kthsocial.com/"
                           ++ t++"/"
                           ++ show ( i * i)

-- | Generate a list of hashtags
tagList :: Gen PSQLTYPE
tagList = do
  n <- chooseInt (1,20)
  ts <- unique n (elements tag)
  pure . psqlVarchar $ intercalate " " ts

-- | Generate a random address
place :: Gen PSQLTYPE
place = do
  p <- elements address
  pure . psqlVarchar $ p

-- | Generate a random image filter
imageFilter = do
  f <- elements imagefilter
  pure . psqlVarchar $ f

--------------------------------------------------------------------------------
-- * text generators and helpers
--------------------------------------------------------------------------------
-- helper not exported
toSentance :: [String] -> String
toSentance l = toUpperFirst . (map toLower) . (intercalate " ") $ l
-- helper not exported
toUpperFirst :: String -> String
toUpperFirst s = (toUpper $ head s) : drop 1 s

-- | Generate random text of the form "I'm feeling <random emotion>"
imfeeling :: Gen PSQLTYPE
imfeeling = do
  e <- elements emotion
  pure . psqlVarchar $ "I'm feeling "++(map toLower e)

-- | Generate random text of the form "That makes me feel <random emotion>"
makesmefeel :: Gen PSQLTYPE
makesmefeel = do
  e <- elements emotion
  pure . psqlVarchar $ "That makes me feel "++(map toLower e)

-- | Generate random text of the form
--   <noun phrase> <predicate> <preposition> <nounphrase>,
--   high risk of sounding unnatural.
simpleSentance :: Gen PSQLTYPE
simpleSentance = do
  np1 <- elements nounphrase
  pred <- elements predicate
  prep <- elements preposition
  np2 <- elements nounphrase
  pure . psqlVarchar . toSentance $ [np1,pred,prep,np2]


-- | Generate random text of the form
--   <noun phrase> <verb phrase> <nounphrase>,
--   sentances are generally of acceptable grammar.
simpleSentance2 :: Gen PSQLTYPE
simpleSentance2 = do
  np1 <- elements nounphrase
  verb <- elements verb
  np2 <- elements nounphrase
  pure . psqlVarchar . toSentance $ [np1,verb,np2]

-- | Generate random Text of the form
--   "I'm feeling <random emotion> about the
--    <random event title> event"
feelabout :: Gen PSQLTYPE
feelabout = do
  f <- imfeeling
  e <- eventTitle
  pure . psqlVarchar . toSentance $
    [(unVarchar f),"about the",(unVarchar e),"event."]

-- | Choose a random tweet from 'tweets' in 'Data.hs'
tweet :: Gen PSQLTYPE
tweet = do
  t <- elements tweets
  pure . psqlVarchar $ t

-- | Combine sentance generators into paragraphs.
sentanceAnd :: Gen PSQLTYPE -> Gen PSQLTYPE -> Gen PSQLTYPE
sentanceAnd f g = do
  a <- f
  b <- g
  pure . psqlVarchar $ (unVarchar a)++". "++(unVarchar b)++"."

-- | Generate random text from several sentance generators.
--   Risk of wonky grammar.
text :: Gen PSQLTYPE
text = oneof [simpleSentance
             ,simpleSentance2
             ,(sentanceAnd simpleSentance makesmefeel)
             ,(sentanceAnd simpleSentance2 makesmefeel)
             ,imfeeling
             ,feelabout
             ]
-- | Generate random text from several sentance generators.
--   Generally acceptable grammar
goodText :: Gen PSQLTYPE
goodText =  oneof [simpleSentance2
                  ,(sentanceAnd simpleSentance2 makesmefeel)
                  ,imfeeling
                  ,feelabout
                  ,tweet
                  ]
