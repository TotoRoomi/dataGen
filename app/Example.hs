-- |

module Example where
import Test.QuickCheck
import Populator
import Generator
{-
User(UserID, Name)
Friend(UserID, FriendID)
Post(PostID, Date, UserID)
PostTag(PostID, Tag)
ImagePost(PostID, Content, Filter)
TextPost(PostID, Content)
VideoPost(PostID, Content)
Likes(UserID, PostID, Date)
Event(EventID, Place, Date, CreatorID)
UserEvent(UserID, EventID)
Subscription(UserID, Expiration)
Transaction(TransactionID, Date)
-}

exampleUser :: Gen InsertStatement
exampleUser = do
  fn <- firstnames 1
  sn <- lastnames 1
  let name = name2 (head fn) (head sn)
  email <- email2 (head fn) (head sn)
  date <- date 2024
  pure $ insertStatement "user" ["name","email","joinDate"] [name,email,date]

user :: Int -> Gen InsertStatement
user n = do
  primarykeys <- primaryKeys n
  fns <- firstnames n
  lns <- lastnames n
  pure $ statements $ map makeUser (zip3 primarykeys fns lns)
  where
    makeUser (pm,fn,sn) =
      insertStatement "user" ["userId","name"] [pm,name2 fn sn]

friend :: [PSQLTYPE] -> Gen InsertStatement
friend uids = do
  -- at least 2 friends, at most half of all the users
  l <- nonReflexivePairs 2 (length uids `div` 2) uids -- [(uid,fid)]
  pure $ statements $ map f l
  where
    f (uid,fid)= insertStatement "Friend" ["UserId","FriendID"] [uid,fid]

post :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
post pids uids = do
  dates <- make (length uids * 100) $ date 2024
  uids' <- make (length pids) $ elements uids
  pure $ statements $ map f $ zip3 pids dates uids'
  where
    f (pid,d,uid) = insertStatement "Post" ["PostID", "Date", "UserId"] [pid,d,uid]



transaction :: Int-> Gen InsertStatement
transaction n = do
  primaryKeys <- primaryKeys n
  dates <- make n $ date 2024
  pure $ statements $ map f $ zip primaryKeys dates
  where
    f (k,d) = insertStatement "Transaction" ["TransactionID","Date"] [k,d]

subscription :: [PSQLTYPE] -> Gen InsertStatement
subscription uids = do
  n <- chooseInt ((length uids `div` 2), ((length uids * 9 ) `div` 10)) -- mellan hälften och 90% som betalar
  let l = take n uids
  dates <- make n $ date 2024
  pure $ statements $ map f $ zip l dates
  where
    f (id, d) = insertStatement "Subscription" ["UserID","Date"] [id,d]
