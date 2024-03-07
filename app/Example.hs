-- |

module Example where

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
Subscription(UserID, Duration)
Transaction(TransactionID, Date)
-}
