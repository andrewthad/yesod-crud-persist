module Yesod.Crud where

import Prelude
import Control.Applicative
import Data.Maybe

import Yesod.Core
import Database.Persist(Key)
import Network.Wai (pathInfo, requestMethod)
import qualified Data.List as List

import Yesod.Crud.Internal

data Crud master a = Crud
  { _chAdd    :: HandlerT (Crud master a) (HandlerT master IO) Html
  , _chIndex  :: HandlerT (Crud master a) (HandlerT master IO) Html
  , _chEdit   :: Key a -> HandlerT (Crud master a) (HandlerT master IO) Html
  , _chDelete :: Key a -> HandlerT (Crud master a) (HandlerT master IO) Html
  }

-- Dispatch for the crud subsite
instance (Eq (Key a), PathPiece (Key a)) => YesodSubDispatch (Crud master a) (HandlerT master IO) where
  yesodSubDispatch env req = h
    where 
    h = let parsed = parseRoute (pathInfo req, []) 
            helper a = subHelper (fmap toTypedContent a) env parsed req
        in case parsed of
          Just (EditR theId)   -> onlyAllow ["GET","POST"] $ helper $ 
                                  getYesod >>= (\s -> _chEdit s theId)
          Just (DeleteR theId) -> onlyAllow ["GET","POST"] $ helper $ 
                                  getYesod >>= (\s -> _chDelete s theId)
          Just AddR            -> onlyAllow ["GET","POST"] $ helper $
                                  getYesod >>= _chAdd
          Just IndexR          -> onlyAllow ["GET"] $ helper $
                                  getYesod >>= _chIndex
          Nothing              -> notFoundApp
    onlyAllow reqTypes waiApp = if isJust (List.find (== requestMethod req) reqTypes) then waiApp else notFoundApp
    notFoundApp = subHelper (fmap toTypedContent notFoundUnit) env Nothing req
    notFoundUnit = fmap (\() -> ()) notFound

instance (PathPiece (Key a), Eq (Key a)) => RenderRoute (Crud master a) where
  data Route (Crud master a)
    = EditR (Key a)
    | DeleteR (Key a)
    | IndexR
    | AddR
  renderRoute r = noParams $ case r of
    EditR theId   -> ["edit", toPathPiece theId]
    DeleteR theId -> ["delete", toPathPiece theId]
    IndexR        -> ["index"]
    AddR          -> ["add"]
    where noParams xs = (xs,[])

instance (Eq (Key a), PathPiece (Key a)) => ParseRoute (Crud master a) where
  parseRoute (_, (_:_)) = Nothing
  parseRoute (xs, []) = Nothing
    <|> (runSM xs $ pure EditR <* consumeMatchingText "edit" <*> consumeKey)
    <|> (runSM xs $ pure DeleteR <* consumeMatchingText "delete" <*> consumeKey)
    <|> (runSM xs $ pure IndexR <* consumeMatchingText "index")
    <|> (runSM xs $ pure AddR <* consumeMatchingText "add")


deriving instance Eq (Key a) => Eq (Route (Crud master a))
deriving instance Show (Key a) => Show (Route (Crud master a))
deriving instance Read (Key a) => Read (Route (Crud master a))

-- In ChildCrud, c is the child type, and p is the type of the identifier
-- for its parent.
data ChildCrud master p c = ChildCrud
  { _ccAdd    :: p -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  , _ccIndex  :: p -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  , _ccEdit   :: Key c -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  , _ccDelete :: Key c -> HandlerT (ChildCrud master p c) (HandlerT master IO) Html
  }

type HierarchyCrud master a = ChildCrud master (Maybe (Key a)) a

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



