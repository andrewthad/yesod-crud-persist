module Yesod.Crud where

import Prelude
import Control.Applicative
import Control.Monad
import Data.Maybe

import Yesod.Core
import Database.Persist(Key)
import Control.Monad.Trans.State (StateT, evalStateT, put, get)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Functor.Identity
import Data.Text (Text)
import Network.Wai (pathInfo, requestMethod)
import qualified Data.List as List

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
    <|> (run $ pure EditR <* consumeMatchingText "edit" <*> consumeKey)
    <|> (run $ pure DeleteR <* consumeMatchingText "delete" <*> consumeKey)
    <|> (run $ pure IndexR <* consumeMatchingText "index")
    <|> (run $ pure AddR <* consumeMatchingText "add")
    where 
    run :: StateT [Text] (MaybeT Identity) (Route (Crud master a)) -> Maybe (Route (Crud master a))
    run a = runIdentity $ runMaybeT $ evalStateT (a <* forceEmpty) xs
    consumeMatchingText t = do
      p <- attemptTakeNextPiece
      guard $ p == t
    consumeKey = do
      t <- attemptTakeNextPiece
      case fromPathPiece t of
        Nothing -> mzero
        Just a  -> return a
    attemptTakeNextPiece = do
      s <- get
      case s of
        (a:as) -> put as >> return a
        [] -> mzero
    forceEmpty = do
      s <- get
      case s of
        [] -> return ()
        _  -> mzero
deriving instance Eq (Key a) => Eq (Route (Crud master a))
deriving instance Show (Key a) => Show (Route (Crud master a))
deriving instance Read (Key a) => Read (Route (Crud master a))

