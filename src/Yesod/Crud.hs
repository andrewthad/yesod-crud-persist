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
import qualified Data.List as List
import qualified Database.Esqueleto as E

import Yesod.OldCrud.Internal
import Data.Typeable
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

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
  -> (CrudRoute p c -> Route site) 
  -> CrudRoute p c
  -> Text
  -> (Entity c -> Text)
  -> (Key c -> YesodDB site p)
  -> (p -> YesodDB site (Maybe (Route site))) 
  -> HandlerT site IO (Text, Maybe (Route site))
breadcrumbsCrud editParent tp route indexName getName getParent getIndexParent = case route of
  AddR p -> return ("Add", Just $ tp $ IndexR p)
  IndexR p -> do
    indexParent <- runDB $ getIndexParent p
    return (indexName, indexParent)
  ViewR cid -> do
    c <- runDB $ get404 cid
    p <- runDB $ getParent cid
    return (getName (Entity cid c), Just $ tp $ IndexR p)
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
  )
