module Yesod.Crud.Simple.Generic where

import Prelude
import Yesod.Crud.Simple
import Yesod.Core
import Yesod.Form
import Yesod.Form.Bootstrap3
import GHC.Generics
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isLower)

class GCrud c where
  gcrudForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => Maybe c -> AForm m c

-- instance GCrud (U1 p) where
--   gcrudForm _ = return (FormSuccess U1, mempty)

instance (GCrud (f p), GCrud (g p)) => GCrud ((:*:) f g p) where
  gcrudForm m = case m of
    Just (a :*: b) -> (:*:) <$> gcrudForm (Just a) <*> gcrudForm (Just b)
    Nothing ->        (:*:) <$> gcrudForm Nothing <*> gcrudForm Nothing

instance (GCrud (f p)) => GCrud (M1 D b f p) where
  gcrudForm m = fmap M1 . gcrudForm $ fmap unM1 m

instance (GCrud (f p)) => GCrud (M1 C b f p) where
  gcrudForm m = fmap M1 . gcrudForm $ fmap unM1 m

instance (Selector b, GCrudNamed c) => GCrud (M1 S b (K1 R c) p) where
  gcrudForm m = let lbl = Text.pack $ selName (undefined :: M1 S b (K1 R c) ()) in
    fmap (M1 . K1) $ case m of
      Just (M1 (K1 c)) -> gcrudFormNamed lbl (Just c)
      Nothing -> gcrudFormNamed lbl Nothing

class GCrudNamed c where
  gcrudFormNamed :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => Text -> Maybe c -> AForm m c

instance GCrudNamed Text where
  gcrudFormNamed lbl m = areq textField (bfs $ Text.dropWhile isLower lbl) m

applyGenericForm :: (Generic c, GCrud (Rep c ()), RenderMessage master FormMessage) => SimpleCrud master p c -> SimpleCrud master p c
applyGenericForm sc = sc
  { _scForm = \m -> renderBootstrap3 BootstrapBasicForm $ id
      <$> (fmap to (gcrudForm (fmap from' m))) 
      <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
  }
  where from' :: Generic a => a -> Rep a ()
        from' = from  -- A hack to stop the type checker from whining about p

