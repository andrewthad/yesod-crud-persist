module Yesod.Crud where

import Prelude
import Control.Applicative
import Data.Maybe

import Lens.Micro.TH

import Yesod.Core
import Database.Persist(Key)
import Network.Wai (pathInfo, requestMethod)
import qualified Data.List as List
import Yesod.Persist
import Database.Persist.Sql
import Data.Foldable (for_)

import Yesod.Crud.Internal

type Crud master a = ChildCrud master () a

-- In ChildCrud, c is the child type, and p is the type of the identifier
-- for its parent.
data ChildCrud master p c = ChildCrud
  { _ccAdd    :: p -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  , _ccIndex  :: p -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  , _ccEdit   :: Key c -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  , _ccDelete :: Key c -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  }
makeLenses ''ChildCrud

-- Dispatch for the child crud subsite
instance (Eq (Key c), PathPiece (Key c), Eq p, PathPiece p) => YesodSubDispatch (ChildCrud master p c) (HandlerT master IO) where
  yesodSubDispatch env req = h
    where 
    h = let parsed = parseRoute (pathInfo req, []) 
            helper a = subHelper (fmap toTypedContent a) env parsed req
        in case parsed of
          Just (ChildEditR theId)   -> onlyAllow ["GET","POST"]
            $ helper $ getYesod >>= (\s -> _ccEdit s theId)
          Just (ChildDeleteR theId) -> onlyAllow ["GET","POST"] 
            $ helper $ getYesod >>= (\s -> _ccDelete s theId)
          Just (ChildAddR p) -> onlyAllow ["GET","POST"] 
            $ helper $ getYesod >>= (\s -> _ccAdd s p)
          Just (ChildIndexR p) -> onlyAllow ["GET"] 
            $ helper $ getYesod >>= (\s -> _ccIndex s p)
          Nothing              -> notFoundApp
    onlyAllow reqTypes waiApp = if isJust (List.find (== requestMethod req) reqTypes) then waiApp else notFoundApp
    notFoundApp = subHelper (fmap toTypedContent notFoundUnit) env Nothing req
    notFoundUnit = fmap (\() -> ()) notFound

instance (PathPiece (Key c), Eq (Key c), PathPiece p, Eq p) => RenderRoute (ChildCrud master p c) where
  data Route (ChildCrud master p c)
    = ChildEditR (Key c)
    | ChildDeleteR (Key c)
    | ChildIndexR p
    | ChildAddR p
  renderRoute r = noParams $ case r of
    ChildEditR theId   -> ["edit",   toPathPiece theId]
    ChildDeleteR theId -> ["delete", toPathPiece theId]
    ChildIndexR p      -> ["index",  toPathPiece p]
    ChildAddR p        -> ["add",    toPathPiece p]
    where noParams xs = (xs,[])

instance (PathPiece (Key c), Eq (Key c), PathPiece p, Eq p) => ParseRoute (ChildCrud master p c) where
  parseRoute (_, (_:_)) = Nothing
  parseRoute (xs, []) = Nothing
    <|> (runSM xs $ pure ChildEditR <* consumeMatchingText "edit" <*> consumeKey)
    <|> (runSM xs $ pure ChildDeleteR <* consumeMatchingText "delete" <*> consumeKey)
    <|> (runSM xs $ pure ChildIndexR <* consumeMatchingText "index" <*> consumeKey)
    <|> (runSM xs $ pure ChildAddR <* consumeMatchingText "add" <*> consumeKey)

deriving instance (Eq (Key c), Eq p) => Eq (Route (ChildCrud master p c))
deriving instance (Show (Key c), Show p) => Show (Route (ChildCrud master p c))
deriving instance (Read (Key c), Read p) => Read (Route (ChildCrud master p c))

type HierarchyCrud master a = ChildCrud master (Maybe (Key a)) a

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


