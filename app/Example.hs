-- |

module Example where
import Test.QuickCheck
import Populator
import Generator
{-
X User(UserID, Name)
X Friend(UserID, FriendID)
X Post(PostID, Date, UserID)
X PostTag(PostID, Tag)
X ImagePost(PostID, Content, Filter)
X TextPost(PostID, Content)
X VideoPost(PostID, Content)
Likes(UserID, PostID, Date)
Event(EventID, Place, Date, CreatorID)
UserEvent(UserID, EventID)
X Subscription(UserID, Expiration)
X Transaction(TransactionID, Date)
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
  l <- pairs (2, (length uids `div` 2)) uids -- [(uid,fid)]
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

postTag :: [PSQLTYPE] -> Gen InsertStatement
postTag pids = do
  tags <- sequence (replicate (length pids) tagList)
  pure $ statements $ map f $ zip pids tags
  where
    f (id, tag) = insertStatement "PostTag" ["PostID","Tag"] [id,tag]

postType :: String -> [PSQLTYPE] -> Gen InsertStatement
postType posttype pids = do
  contents <- mapM (url posttype) pids
  pure $ statements $ map f $ zip pids contents
  where
    f (id, content) = insertStatement "TextPost" ["PostID","Content"] [id,content]

textPost :: [PSQLTYPE] -> Gen InsertStatement
textPost pids = postType "Text" pids

imagePost :: [PSQLTYPE] -> Gen InsertStatement
imagePost pids = postType "Image" pids

videoPost :: [PSQLTYPE] -> Gen InsertStatement
videoPost pids = postType "Video" pids

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
