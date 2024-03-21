-- |

module Example where
import Test.QuickCheck
import Populator
import Generator
import Data.List(zip4)
import Pretty
{-
 User(UserID, Name)
 Friend(UserID, FriendID)
 Post(PostID, Date, UserID)
 PostTag(PostID, Tag)
 ImagePost(PostID, Content, Filter)
 TextPost(PostID, Content)
 VideoPost(PostID, Content)
 Likes(UserID, PostID, Date)
 Event(EventID, Place, SDate, EDate, CreatorID, Title)
 UserEvent(UserID, EventID)
 Subscription(UserID, Expiration)
 Transaction(TransactionID, Date)
-}

printAllInserts = do
  -- All primaryKeys
  userIDs <- generate $ primaryKeys 100
  postIDs <- generate $ primaryKeys 1000
  eventIDs <- generate $ primaryKeys 25
  -- Generate inserts
  pretty $ user userIDs
  pretty $  friend userIDs
  pretty $ post postIDs userIDs
  pretty $ postTag postIDs
  pretty $ textPost (take 500 postIDs)
  pretty $ imagePost (take 300 $ drop 500 postIDs)
  pretty $ videoPost (drop 800 postIDs)
  pretty $ likes userIDs postIDs
  pretty $ event eventIDs userIDs
  pretty $ userEvent userIDs eventIDs
  pretty $ subscription userIDs
  pretty $ transaction 100


exampleUser :: Gen InsertStatement
exampleUser = do
  fn <- firstnames 1
  sn <- lastnames 1
  let name = name2 (head fn) (head sn)
  email <- email2 (head fn) (head sn)
  date <- date 2024
  pure $ insertStatement "user" ["name","email","joinDate"] [name,email,date]

user :: [PSQLTYPE] -> Gen InsertStatement
user uids = do
  let n = length uids
  fns <- firstnames n
  lns <- lastnames n
  let names = zipWith name2 fns lns
  pure $ insert "user" ["userId","name"] [uids,names]

friend :: [PSQLTYPE] -> Gen InsertStatement
friend uids = do
  -- at least 2 friends, at most half of all the users
  l <- pairs (2, (length uids `div` 2)) uids -- [[fid][uid]]
  pure $ insert "Friend" ["UserId","FriendID"] l
  
post :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
post pids uids = do
  dates <- make (length uids * 100) $ date 2024
  uids' <- make (length pids) $ elements uids
  pure $ insert "Post" ["PostID", "Date", "UserId"] [pids, dates, uids']

postTag :: [PSQLTYPE] -> Gen InsertStatement
postTag pids = do
  tags <- sequence (replicate (length pids) tagList)
  pure $ insert "PostTag" ["PostID","Tag"] [pids,tags]

postType :: String -> [PSQLTYPE] -> Gen InsertStatement
postType posttype pids = do
  contents <- mapM (url posttype) pids
  pure $ insert "TextPost" ["PostID","Content"] [pids,contents]

textPost :: [PSQLTYPE] -> Gen InsertStatement
textPost pids = postType "Text" pids

imagePost :: [PSQLTYPE] -> Gen InsertStatement
imagePost pids = postType "Image" pids

videoPost :: [PSQLTYPE] -> Gen InsertStatement
videoPost pids = postType "Video" pids

likes :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
likes uids pids = do
  ps <- pairs2' pids uids -- [(uid,pid)]
  dates <- make (length ps) $ dateBetween (2024,1,1) (2024,12,31) -- [PSQLTYPE]
  let (pids', uids') = unzip ps
  pure $ insert "Likes" ["UserID", "PostID", "Date"] [uids,pids,dates]

-- Event(EventID, Place, SDate, EDate, CreatorID, Title)
event :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
event eids uids = do
  ps <- pairs2' eids uids
  dates <- make (length ps) $ dateBetween (2024,1,1) (2024,12,31) -- [PSQLTYPE]
  places <- make (length ps) $ place
  let (eids', uids') = unzip ps
  pure $ insert "Event" ["EventID","Place","SDate", "EDate","CreatorID", "Title"] [eids, places, dates, uids]

userEvent :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
userEvent uids eids = do
  pairs <- pairs2 (1,((length uids * 7) `div` 10)) eids uids
  pure $ insert "userEvent" ["UserId","EventId"] pairs

subscription :: [PSQLTYPE] -> Gen InsertStatement
subscription uids = do
  n <- chooseInt ((length uids `div` 2), ((length uids * 9 ) `div` 10)) -- mellan hälften och 90% som betalar
  let uids' = take n uids
  dates <- make n $ date 2024
  pure $ insert "Subscription" ["UserID","Date"] [uids', dates]

transaction :: Int -> Gen InsertStatement
transaction n = do
  tids <- primaryKeys n
  dates <- make n $ date 2024
  pure $ insert "Transaction" ["TransactionID","Date"] [tids, dates]
