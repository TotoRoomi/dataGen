-- |

module Example where
import Test.QuickCheck
import Populator
import Generator
import Pretty
{-
 User(UserID, Name)
 Friend(UserID, FriendID)
 Post(PostID, Date, UserID)
 PostTag(PostID, Tag)
 ImagePost(PostID, URL, Filter)
 TextPost(PostID, Text)
 VideoPost(PostID, URL, Codec)
 Likes(UserID, PostID, Date)
 Event(EventID, Place, SDate, EDate, CreatorID, Title)
 UserEvent(UserID, EventID)
 Subscription(UserID, Expiration)
 Transaction(TransactionID, Date)
-}

printAllInserts = do
  -- Setup
  userIDs <- generate $ primaryKeys 100
  postIDs <- generate $ primaryKeys 1000
  eventIDs <- generate $ primaryKeys 25
  postDates <- generate $ make 1000 $ dateBetween (2024,1,1) (2024,12,1)
  -- Generate inserts
  pretty $ user userIDs
  pretty $  friend userIDs
  pretty $ post postIDs userIDs -- date
  pretty $ postTag postIDs
  pretty $ textPost (take 500 postIDs)
  pretty $ imagePost (take 300 $ drop 500 postIDs)
  pretty $ videoPost (drop 800 postIDs)
  pretty $ likes userIDs postIDs postDates -- date -- MAP TO POSTS
  pretty $ event eventIDs userIDs --date
  pretty $ attending userIDs eventIDs
  pretty $ subscription userIDs --date
  pretty $ transaction 100 --date


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

textPost :: [PSQLTYPE] -> Gen InsertStatement
textPost pids = do
  texts <- make (length pids) goodText
  pure $ insert "TextPost" ["PostID","Text"] [pids,texts]

imagePost :: [PSQLTYPE] -> Gen InsertStatement
imagePost pids = do
  urls <- mapM (url "image") pids
  filters <- make (length pids) imageFilter
  pure $ insert "ImagePost" ["PostID","URL","Filter"] [pids,urls,filters]

videoPost :: [PSQLTYPE] -> Gen InsertStatement
videoPost pids = do
  urls <-  mapM (url "video") pids
  cs <- make (length pids) $ elements ["AVC"
                                      ,"H.264" -- AVC
                                      ,"HEVC"
                                      ,"H.265" -- HEVC
                                      ,"VP9"
                                      ,"AV1"
                                      ,"MPEG-4"
                                      ,"Theora"
                                      ,"VP8"]
  let codecs = map psqlVarchar cs
  pure $ insert "VideoPost" ["PostID", "URL", "Codec"] [pids,urls,codecs]

likes' :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
likes' uids pids = do
  ps <- pairs2' pids uids -- [(uid,pid)]
  dates <- make (length ps) $ dateBetween (2024,1,1) (2024,12,31) -- [PSQLTYPE]
  let (pids', uids') = unzip ps
  pure $ insert "Likes" ["UserID", "PostID", "Date"] [uids,pids,dates]

likes :: [PSQLTYPE] -> [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
likes uids pids ds = do
  (ns, pairLists) <- forEachKeyMakePairs (0,length uids) pids uids
  d <- forEachDateMakeDates ds ns
  let dates = concat d
  let uids' = head . tail $ pairLists
  let pids' = head pairLists
  pure $ insert "Likes" ["UserID", "PostID", "Date"] [uids',pids',dates]


-- Event(EventID, Place, SDate, EDate, CreatorID, Title)
event :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
event eids uids = do
  ps <- pairs2' eids uids
  let n = length ps
  ds <- make n $ eventTimestampBetween (2024,1,1) (2024,12,31) -- [PSQLTYPE]
  let (sdates,edates) = unzip ds
  places <- make n $ place
  let (eids', uids') = unzip ps
  titles <- make n eventTitle
  pure $ insert "Event" ["EventID","Place","SDate", "EDate","CreatorID", "Title"] [eids, places, sdates, edates, uids, titles]

attending :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
attending uids eids = do
  pairs <- pairs2 (1,((length uids * 7) `div` 10)) eids uids
  pure $ insert "Attending" ["UserId","EventId"] pairs

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
