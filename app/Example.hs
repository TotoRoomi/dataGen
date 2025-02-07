-- | Creates insert statements and prints them for the
--   following example schemas:
-- User(UserID, Name)
-- Friend(UserID, FriendID)
-- Post(PostID, Date, UserID, Title, Place)
-- PostTag(PostID, Tag)
-- ImagePost(PostID, URL, Filter)
-- TextPost(PostID, Text)
-- VideoPost(PostID, URL, Codec)
-- Likes(UserID, PostID, Date)
-- Event(EventID, Place, SDate, EDate, CreatorID, Title)
-- UserEvent(UserID, EventID)
-- Subscription(UserID, Expiration)
-- Transaction(TransactionID, Date)

module Example where
import Test.QuickCheck
import Populator
import Generator
import Pretty

import Control.Monad ( replicateM ) 


-- | Pretty prints the insert statements in the
--   format that works in PSQL
printAllInserts = do
  -- Setup
  userIDs <- generate $ primaryKeys 100
  postIDs <- generate $ primaryKeys 1000
  eventIDs <- generate $ primaryKeys 25
  postDates <- generate $ make 1000 $ dateBetween (2024,3,28) (2024,12,23)

  -- Generate inserts
  pretty $ user userIDs
  pretty $ friend userIDs
  pretty $ post postIDs userIDs postDates 
  pretty $ postTag postIDs
  pretty $ textPost (take 500 postIDs)
  pretty $ imagePost (take 300 $ drop 500 postIDs)
  pretty $ videoPost (drop 800 postIDs)
  pretty $ likes userIDs postIDs postDates
  pretty $ event eventIDs userIDs
  pretty $ attending userIDs eventIDs
  pretty $ subscription userIDs
  pretty $ transaction userIDs


-- | User(UserID, Name)
user :: [PSQLTYPE] -> Gen InsertStatement
user uids = do
  let n = length uids
  fns <- firstnames n
  lns <- lastnames n
  let names = zipWith name2 fns lns
  pure $ insert "users" ["userId","name"] [uids,names]

-- | Friend(UserID, FriendID)
friend :: [PSQLTYPE] -> Gen InsertStatement
friend uids = do
  l <- selfRefPairs' ((length uids) `div` 2) uids
  pure $ insert "Friend" ["UserId","FriendID"] l


-- | Post(PostID, Date, UserID, Title, Place)
post :: [PSQLTYPE] -> [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
post pids uids dates = do
  uids' <- make (length pids) $ elements uids
  places <- make (length pids) place
  titles <- make (length pids) postTitle
  pure $ insert "Post" ["PostID", "Date", "UserId", "Title", "Place"] [pids, dates, uids', titles, places]

-- | PostTag(PostID, Tag)
postTag :: [PSQLTYPE] -> Gen InsertStatement
postTag pids = do
  tags <- make ( (length pids) * 3) $ tagGen
  pars <- forEachKeyMakePairs' (1,20) pids tags
  pure $ insert "PostTag" ["PostID","Tag"] pars

-- | TextPost(PostID, Text)
textPost :: [PSQLTYPE] -> Gen InsertStatement
textPost pids = do
  texts <- make (length pids) goodText
  pure $ insert "TextPost" ["PostID","Text"] [pids,texts]

-- | ImagePost(PostID, URL, Filter)
imagePost :: [PSQLTYPE] -> Gen InsertStatement
imagePost pids = do
  urls <- mapM (url "image") pids
  filters <- make (length pids) imageFilter
  pure $ insert "ImagePost" ["PostID","URL","Filter"] [pids,urls,filters]

-- | VideoPost(PostID, URL, Codec)
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
  dates <- make (length ps) $ dateBetween (2024,3,28) (2024,12,23) -- [PSQLTYPE]
  let (pids', uids') = unzip ps
  pure $ insert "Likes" ["UserID", "PostID", "Date"] [uids,pids,dates]

-- | Likes(UserID, PostID, Date)
likes :: [PSQLTYPE] -> [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
likes uids pids ds = do
  (ns, pairLists) <- forEachKeyMakePairs (0,length uids) pids uids
  d <- forEachDateMakeDates ds ns
  let dates = concat d
  let uids' = head . tail $ pairLists
  let pids' = head pairLists
  pure $ insert "Likes" ["UserID", "PostID", "Date"] [uids',pids',dates]


-- | Event(EventID, Place, SDate, EDate, CreatorID, Title)
event :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
event eids uids = do
  ps <- pairs2' eids uids
  let n = length ps
  ds <- make n $ eventTimestampBetween (2024,3,28) (2024,12,23)
  let (sdates,edates) = unzip ds
  places <- make n $ place
  let (eids', uids') = unzip ps
  titles <- make n eventTitle
  pure $ insert "Event" ["EventID","Place","SDate", "EDate","CreatorID", "Title"] [eids, places, sdates, edates, uids, titles]

-- | attending(UserID, EventID)
attending :: [PSQLTYPE] -> [PSQLTYPE] -> Gen InsertStatement
attending uids eids = do
  pairs <- forEachKeyMakePairs' (1,((length uids * 7) `div` 10)) eids uids
  pure $ insert "Attending" ["UserId","EventId"] [head . tail $ pairs ,head pairs]

-- | Subscription(UserID, Expiration)
subscription :: [PSQLTYPE] -> Gen InsertStatement
subscription uids = do
  n <- chooseInt ((length uids `div` 2), ((length uids * 9 ) `div` 10))
  let uids' = take n uids
  dates <- make n $ date 2024
  pure $ insert "Subscription" ["UserID","expiration"] [uids', dates]

-- | Transaction(TransactionID, Date)
transaction :: [PSQLTYPE] -> Gen InsertStatement
transaction tids = do
  listOfDateList <- mapM genDatesPerUser tids --[ ([id],[(y,m,d)...]) ]
  let (idList, datesList) = unzip listOfDateList -- ( [[id]], [[date]])
  let allIds = concat idList 
  let allDates = map psqlDate $ concat datesList 
  pure $ insert "Transaction" ["TransactionID","Date"] [allIds, allDates]
  where
  --([id],[(y,m,d)...])
    genDatesPerUser id = do
      ranStartMonth <- chooseInt(1,3)
      let nrMonths = 13 - ranStartMonth
      days <- replicateM nrMonths (chooseInt(23,28))
      let months = [ranStartMonth .. 12]
          years = repeat 2024
      pure $ (replicate nrMonths id, zip3 years months days)