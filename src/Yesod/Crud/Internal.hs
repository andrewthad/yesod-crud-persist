module Yesod.Crud.Internal where

import Control.Monad.Trans.State (StateT, evalStateT, put, get)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Functor.Identity
import Control.Monad
import Yesod.Core
import Control.Applicative
import Data.Text (Text)

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
  s <- get
  case s of
    (a:as) -> put as >> return a
    [] -> mzero

forceEmpty :: StateT [Text] (MaybeT Identity) ()
forceEmpty = do
  s <- get
  case s of
    [] -> return ()
    _  -> mzero

-- import ClassyPrelude.Yesod
-- import Yesod.Core
-- import Yesod.Core.Types
-- import qualified Network.Wai as W
-- 
-- subHelper :: Monad m 
--           => HandlerT child (HandlerT parent m) TypedContent
--           -> YesodSubRunnerEnv child parent (HandlerT parent m)
--           -> Maybe (Route child)
--           -> W.Application
-- subHelper handlert YesodSubRunnerEnv {..} route =
--     ysreParentRunner base ysreParentEnv (fmap ysreToParentRoute route)
--   where
--     base = stripHandlerT (fmap toTypedContent handlert) ysreGetSub ysreToParentRoute route
-- 
-- stripHandlerT :: HandlerT child (HandlerT parent m) a
--               -> (parent -> child)
--               -> (Route child -> Route parent)
--               -> Maybe (Route child)
--               -> HandlerT parent m a
-- stripHandlerT (HandlerT f) getSub toMaster newRoute = HandlerT $ \hd -> do
--     let env = handlerEnv hd
--     ($ hd) $ unHandlerT $ f hd
--         { handlerEnv = env
--             { rheSite = getSub $ rheSite env
--             , rheRoute = newRoute
--             , rheRender = \url params -> rheRender env (toMaster url) params
--             }
--         , handlerToParent = toMaster
--         }
