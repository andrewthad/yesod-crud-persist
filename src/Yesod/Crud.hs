module Yesod.Crud where

import Prelude
import Control.Applicative
import Data.Maybe

import Lens.Micro.TH

import Yesod.Core
import Database.Persist(Key)
import Network.Wai (pathInfo, requestMethod)
import Yesod.Persist
import Database.Persist.Sql
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Monoid
import qualified Data.List as List
import qualified Database.Esqueleto as E

import Yesod.Crud.Internal
import Data.Typeable
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

-- In Crud, c is the child type, and p is the type of the identifier
-- for its parent.
data Crud master p c = Crud
  { _ccAdd    :: p -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _ccIndex  :: p -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _ccEdit   :: Key c -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _ccDelete :: Key c -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _ccView   :: Key c -> HandlerT (Crud master p c) (HandlerT master IO) Html
  }
makeLenses ''Crud

breadcrumbsCrud :: PersistCrudEntity master c
                => (Route (Crud master p c) -> Route master) 
                -> Route (Crud master p c) 
                -> Text
                -> (c -> Text)
                -> (Key c -> YesodDB master p)
                -> (p -> HandlerT master IO (Maybe (Route master))) 
                -> HandlerT master IO (Text, Maybe (Route master))
breadcrumbsCrud tp route indexName getName getParent getIndexParent = case route of
  AddR p -> return ("Add", Just $ tp $ IndexR p)
  IndexR p -> do
    indexParent <- getIndexParent p
    return (indexName, indexParent)
  ViewR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ getParent cid
    return ("View - " <> getName c, Just $ tp $ IndexR p)
  EditR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ getParent cid
    return ("Edit - " <> getName c, Just $ tp $ IndexR p)
  DeleteR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ getParent cid
    return ("Delete - " <> getName c, Just $ tp $ IndexR p)

breadcrumbsCrudHierarchy :: (PersistCrudEntity master a, SqlClosure a c)
  => (Route (Crud master (Maybe (Key a)) a) -> Route master) 
  -> Route (Crud master (Maybe (Key a)) a) 
  -> Text
  -> Maybe (Route master)
  -> (a -> Text)
  -> HandlerT master IO (Text, Maybe (Route master))
breadcrumbsCrudHierarchy tp route indexName parent getName = case route of
  AddR p -> return ("Add", Just $ tp $ IndexR p)
  ViewR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ closureGetParentId cid
    return ("View - " <> getName c, Just $ tp $ IndexR p)
  EditR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ closureGetParentId cid
    return ("Edit - " <> getName c, Just $ tp $ IndexR p)
  DeleteR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ closureGetParentId cid
    return ("Delete - " <> getName c, Just $ tp $ IndexR p)
  IndexR Nothing -> return (indexName, parent)
  IndexR (Just cid) -> do
    c <- runDB $ get404 cid
    p <- runDB $ closureGetParentId cid
    return (getName c, Just . tp . IndexR $ p)

-- By using this, you will trade some type safety
data SomeCrud master = forall p c. (Typeable p, Typeable c) => SomeCrud (Crud master p c)

findCrud :: forall p c master. (Typeable p, Typeable c) => [SomeCrud master] -> Crud master p c
findCrud = fromJust . go 
  where
  go (SomeCrud (crud :: Crud master p1 c1):cs) = 
    if typeRep (Proxy :: Proxy p1) == typeRep (Proxy :: Proxy p)
       && typeRep (Proxy :: Proxy c1) == typeRep (Proxy :: Proxy c)
      then Just (unsafeCoerce crud)
      else go cs
  go [] = Nothing

-- Dispatch for the child crud subsite
instance (Eq (Key c), PathPiece (Key c), Eq p, PathPiece p) => YesodSubDispatch (Crud master p c) (HandlerT master IO) where
  yesodSubDispatch env req = h
    where 
    h = let parsed = parseRoute (pathInfo req, []) 
            helper a = subHelper (fmap toTypedContent a) env parsed req
        in case parsed of
          Just (EditR theId)   -> onlyAllow ["GET","POST"]
            $ helper $ getYesod >>= (\s -> _ccEdit s theId)
          Just (DeleteR theId) -> onlyAllow ["GET","POST"] 
            $ helper $ getYesod >>= (\s -> _ccDelete s theId)
          Just (AddR p) -> onlyAllow ["GET","POST"] 
            $ helper $ getYesod >>= (\s -> _ccAdd s p)
          Just (IndexR p) -> onlyAllow ["GET"] 
            $ helper $ getYesod >>= (\s -> _ccIndex s p)
          Just (ViewR theId) -> onlyAllow ["GET"] 
            $ helper $ getYesod >>= (\s -> _ccView s theId)
          Nothing              -> notFoundApp
    onlyAllow reqTypes waiApp = if isJust (List.find (== requestMethod req) reqTypes) then waiApp else notFoundApp
    notFoundApp = subHelper (fmap toTypedContent notFoundUnit) env Nothing req
    notFoundUnit = fmap (\() -> ()) notFound

instance (PathPiece (Key c), Eq (Key c), PathPiece p, Eq p) => RenderRoute (Crud master p c) where
  data Route (Crud master p c)
    = EditR (Key c)
    | DeleteR (Key c)
    | IndexR p
    | AddR p
    | ViewR (Key c)
  renderRoute r = noParams $ case r of
    EditR theId   -> ["edit",   toPathPiece theId]
    DeleteR theId -> ["delete", toPathPiece theId]
    IndexR p      -> ["index",  toPathPiece p]
    AddR p        -> ["add",    toPathPiece p]
    ViewR theId   -> ["view",   toPathPiece theId]
    where noParams xs = (xs,[])

instance (PathPiece (Key c), Eq (Key c), PathPiece p, Eq p) => ParseRoute (Crud master p c) where
  parseRoute (_, (_:_)) = Nothing
  parseRoute (xs, []) = Nothing
    <|> (runSM xs $ pure EditR <* consumeMatchingText "edit" <*> consumeKey)
    <|> (runSM xs $ pure DeleteR <* consumeMatchingText "delete" <*> consumeKey)
    <|> (runSM xs $ pure IndexR <* consumeMatchingText "index" <*> consumeKey)
    <|> (runSM xs $ pure AddR <* consumeMatchingText "add" <*> consumeKey)
    <|> (runSM xs $ pure ViewR <* consumeMatchingText "view" <*> consumeKey)

deriving instance (Eq (Key c), Eq p) => Eq (Route (Crud master p c))
deriving instance (Show (Key c), Show p) => Show (Route (Crud master p c))
deriving instance (Read (Key c), Read p) => Read (Route (Crud master p c))

type HierarchyCrud master a = Crud master (Maybe (Key a)) a

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

type PersistCrudEntity master a =
  ( PathPiece (Key a) 
  , Yesod master
  , YesodPersist master
  , PersistEntity a
  , PersistQuery (YesodPersistBackend master)
  , PersistEntityBackend a ~ YesodPersistBackend master
  )

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

