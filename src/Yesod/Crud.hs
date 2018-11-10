module Yesod.Crud where

import Prelude
import Control.Applicative
import Data.Maybe

import Lens.Micro.TH

import Yesod.Core
import Database.Persist (Key)
import Network.Wai (pathInfo, requestMethod)
import Yesod.Persist hiding (Key)
import Database.Persist.Sql
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Monoid
import Data.Functor.Identity
import Control.Monad
import qualified Data.List as List
import qualified Database.Esqueleto as E

import Data.Typeable
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import qualified Control.Monad.Trans.State.Strict as S

-- In Crud, c is the child type, and p is the type of the identifier
-- for its parent.
data CrudHandler site p c = CrudHandler
  { _chAdd    :: p -> HandlerT site IO Html
  , _chIndex  :: p -> HandlerT site IO Html
  , _chEdit   :: Key c -> HandlerT site IO Html
  , _chDelete :: Key c -> HandlerT site IO Html
  , _chView   :: Key c -> HandlerT site IO Html
  }
makeLenses ''CrudHandler

data CrudRoute p c
  = AddR p
  | IndexR p
  | EditR (Key c)
  | DeleteR (Key c)
  | ViewR (Key c)

data EditParent = EditParentView | EditParentIndex

data ViewParent site p
  = ViewParentIndex 
  | ViewParentOther (p -> Route site)

handleCrud :: CrudHandler site p c -> CrudRoute p c -> HandlerT site IO Html
handleCrud (CrudHandler add index edit delete view) route = case route of
  AddR    p -> add p
  IndexR  p -> index p
  EditR   p -> edit p
  DeleteR p -> delete p
  ViewR   p -> view p

instance (PathPiece (Key c), Eq (Key c), PathPiece p, Eq p) => PathMultiPiece (CrudRoute p c) where
  fromPathMultiPiece xs = Nothing
    <|> (runSM xs $ pure EditR   <* consumeMatchingText "edit" <*> consumeKey)
    <|> (runSM xs $ pure DeleteR <* consumeMatchingText "delete" <*> consumeKey)
    <|> (runSM xs $ pure IndexR  <* consumeMatchingText "index" <*> consumeKey)
    <|> (runSM xs $ pure AddR    <* consumeMatchingText "add" <*> consumeKey)
    <|> (runSM xs $ pure ViewR   <* consumeMatchingText "view" <*> consumeKey)
  toPathMultiPiece r = case r of
    EditR theId   -> ["edit",   toPathPiece theId]
    DeleteR theId -> ["delete", toPathPiece theId]
    IndexR p      -> ["index",  toPathPiece p]
    AddR p        -> ["add",    toPathPiece p]
    ViewR theId   -> ["view",   toPathPiece theId]

breadcrumbsCrud :: PersistCrudEntity site c
  => EditParent
  -> ViewParent site p
  -> (CrudRoute p c -> Route site) 
  -> CrudRoute p c
  -> Text
  -> (Entity c -> Text)
  -> (Key c -> YesodDB site p)
  -> (p -> YesodDB site (Maybe (Route site))) 
  -> HandlerT site IO (Text, Maybe (Route site))
breadcrumbsCrud editParent viewParent tp route indexName getName getParent getIndexParent = case route of
  AddR p -> 
    let route = case viewParent of
          ViewParentIndex -> tp $ IndexR p
          ViewParentOther f -> f p
     in return ("Add", Just route)
  IndexR p -> do
    indexParent <- runDB $ getIndexParent p
    return (indexName, indexParent)
  ViewR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ getParent cid
    let route = case viewParent of
          ViewParentIndex -> Just $ tp $ IndexR p
          ViewParentOther f -> Just (f p)
    return (getName (Entity cid c), route)
  EditR cid -> case editParent of
    EditParentView  -> return ("Edit", Just $ tp $ ViewR cid)
    EditParentIndex -> do
      c <- runDB $ get404 cid
      p <- runDB $ getParent cid
      return ("Edit - " <> getName (Entity cid c), Just $ tp $ IndexR p)
  DeleteR cid -> return ("Delete", Just $ tp $ ViewR cid)

deriving instance (Eq (Key c), Eq p) => Eq (CrudRoute p c)
deriving instance (Show (Key c), Show p) => Show (CrudRoute p c)
deriving instance (Read (Key c), Read p) => Read (CrudRoute p c)

type PersistCrudEntity site a =
  ( PathPiece (Key a) 
  , Yesod site
  , YesodPersist site
  , PersistEntity a
  , PersistQuery (YesodPersistBackend site)
  , PersistEntityBackend a ~ YesodPersistBackend site
  , BaseBackend (YesodPersistBackend site) ~ YesodPersistBackend site
  )

class (NodeTable c ~ a, ClosureTable a ~ c) => ClosureTablePair a c where
  type NodeTable c
  type ClosureTable a
  closureAncestorCol :: EntityField c (Key a)
  closureDescendantCol :: EntityField c (Key a)
  closureDepthCol :: EntityField c Int
  closureAncestor :: c -> Key a
  closureDescendant :: c -> Key a
  closureDepth :: c -> Int
  closureCreate :: Key a -> Key a -> Int -> c

type SqlClosure a c = 
  ( ClosureTablePair a c
  , PersistEntityBackend a ~ SqlBackend
  , PersistEntityBackend (ClosureTable a) ~ SqlBackend
  , PersistEntity a
  , PersistEntity c
  , PersistField (Key a)
  )

closureDepthColAs :: forall a c. ClosureTablePair a c 
  => Key a -> EntityField c Int
closureDepthColAs _ = (closureDepthCol :: EntityField c Int)

closureGetRootNodes :: (MonadIO m, SqlClosure a c) => SqlPersistT m [Entity a]
closureGetRootNodes = E.select $ E.from $ \a -> do
  E.where_ $ E.notExists $ E.from $ \c -> do
    E.where_ $ c E.^. closureDescendantCol E.==. a E.^. persistIdField
         E.&&. c E.^. closureDepthCol E.>. E.val 0
  return a

-- This includes the child itself, the root comes first
closureGetParents :: (MonadIO m, SqlClosure a c) => Key a -> SqlPersistT m [Entity a]
closureGetParents theId = E.select $ E.from $ \(a `E.InnerJoin` c) -> do
  E.on $ a E.^. persistIdField E.==. c E.^. closureAncestorCol
  E.where_ $ c E.^. closureDescendantCol E.==. E.val theId
  E.orderBy [E.desc $ c E.^. closureDepthCol]
  return a

closureGetMaybeImmidiateChildren :: (MonadIO m, SqlClosure a c)
  => Maybe (Key a) -> SqlPersistT m [Entity a]
closureGetMaybeImmidiateChildren Nothing = closureGetRootNodes
closureGetMaybeImmidiateChildren (Just k) = closureGetImmidiateChildren k

closureGetImmidiateChildren :: (MonadIO m, SqlClosure a c) 
   => Key a -> SqlPersistT m [Entity a]
closureGetImmidiateChildren theId = do
  cs <- selectList [closureAncestorCol ==. theId, closureDepthCol ==. 1] []
  selectList [persistIdField <-. map (closureDescendant . entityVal) cs] [Asc persistIdField]

closureGetParentId :: (MonadIO m, SqlClosure a c) 
   => Key a -> SqlPersistT m (Maybe (Key a))
closureGetParentId theId = do
  cs <- selectList [closureDescendantCol ==. theId, closureDepthCol ==. 1] []
  return $ fmap (closureAncestor . entityVal) $ listToMaybe cs

closureGetParentIdProxied :: (MonadIO m, SqlClosure a c) 
   => p c -> Key a -> SqlPersistT m (Maybe (Key a))
closureGetParentIdProxied _ = closureGetParentId

closureInsert :: forall m a c. (MonadIO m, SqlClosure a c) 
  => Maybe (Key a) -> a -> SqlPersistT m (Key a)
closureInsert mparent a = do
  childId <- insert a
  _ <- insert $ closureCreate childId childId 0 
  for_ mparent $ \parentId -> do
    cs <- selectList [closureDescendantCol ==. parentId] []
    insertMany_ $ map (\(Entity _ c) -> 
      closureCreate (closureAncestor c) childId (closureDepth c + 1)) cs 
  return childId

closureRootNodes :: (MonadIO m, SqlClosure a c) => SqlPersistT m [Entity a]
closureRootNodes = error "Write this" -- probably with esqueleto

runSM :: [Text] -> StateT [Text] (MaybeT Identity) a -> Maybe a
runSM xs a = runIdentity $ runMaybeT $ evalStateT (a <* forceEmpty) xs

consumeMatchingText :: Text -> StateT [Text] (MaybeT Identity) ()
consumeMatchingText t = do
  p <- attemptTakeNextPiece
  guard $ p == t

consumeKey :: PathPiece k => StateT [Text] (MaybeT Identity) k
consumeKey = do
  t <- attemptTakeNextPiece
  case fromPathPiece t of
    Nothing -> mzero
    Just a  -> return a

attemptTakeNextPiece :: StateT [b] (MaybeT Identity) b
attemptTakeNextPiece = do
  s <- S.get
  case s of
    (a:as) -> S.put as >> return a
    [] -> mzero

forceEmpty :: StateT [Text] (MaybeT Identity) ()
forceEmpty = do
  s <- S.get
  case s of
    [] -> return ()
    _  -> mzero

