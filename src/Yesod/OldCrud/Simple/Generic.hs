module Yesod.OldCrud.Simple.Generic where

import Prelude
import Yesod.OldCrud
import Yesod.OldCrud.Simple
import Yesod.Core
import Yesod.Form
import Yesod.Form.Bootstrap3
import GHC.Generics
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isLower)
import Database.Persist
import Database.Persist.Sql
import Yesod.Persist
import Data.Either.Combinators
import Data.Time
import Yesod.Markdown

class HasName a where
  gcrudName :: a -> Text
  gcrudNameField :: EntityField a Text

class GCrud c where
  gcrudForm :: (Yesod site, YesodPersist site, YesodPersistBackend site ~ SqlBackend, m ~ HandlerT site IO, MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => Maybe c -> AForm m c

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
  gcrudFormNamed :: (Yesod site, YesodPersist site, YesodPersistBackend site ~ SqlBackend, m ~ HandlerT site IO, MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => Text -> Maybe c -> AForm m c

instance GCrudNamed Markdown where
  gcrudFormNamed lbl m = areq markdownField (bfs $ Text.dropWhile isLower lbl) m

instance GCrudNamed Text where
  gcrudFormNamed lbl m = areq textField (bfs $ Text.dropWhile isLower lbl) m

instance GCrudNamed Day where
  gcrudFormNamed lbl m = areq dayField (bfs $ Text.dropWhile isLower lbl) m

instance GCrudNamed [Text] where
  gcrudFormNamed lbl m = areq (convertField (Text.splitOn " ") (Text.intercalate " ") textField) (bfs $ Text.dropWhile isLower lbl) m

instance GCrudNamed Int where
  gcrudFormNamed lbl m = areq intField (bfs $ Text.dropWhile isLower lbl) m

instance (HasName a, Eq (Key a), PathPiece (Key a), PersistEntity a, PersistEntityBackend a ~ SqlBackend) => GCrudNamed (Key a) where
  gcrudFormNamed lbl m = areq (selectField genericNamedOpts) (bfs $ Text.dropWhile isLower lbl) m

applyGenericForm :: (Yesod master, Generic c, GCrud (Rep c ()), RenderMessage master FormMessage, YesodPersist master, YesodPersistBackend master ~ SqlBackend)
  => SimpleCrud master p c -> SimpleCrud master p c
applyGenericForm sc = sc
  { _scForm = \e -> let m = rightToMaybe e in renderBootstrap3 BootstrapBasicForm $ id
      <$> (fmap to (gcrudForm (fmap from' m))) 
      <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
  }
  where from' :: Generic a => a -> Rep a ()
        from' = from  -- A hack to stop the type checker from whining about p

genericForm :: (Yesod master, Generic a, GCrud (Rep a ()), RenderMessage master FormMessage, YesodPersist master, YesodPersistBackend master ~ SqlBackend) 
  => Maybe a -> Html -> MForm (HandlerT master IO) (FormResult a, WidgetT master IO ())
genericForm m = renderBootstrap3 BootstrapBasicForm $ id
  <$> (fmap to (gcrudForm (fmap from' m))) 
  <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
  where from' :: Generic a => a -> Rep a ()
        from' = from  -- A hack to stop the type checker from whining about p

genericIndex :: (PersistCrudEntity site c, HasName c)
  => p -> HandlerT (Crud site p c) (HandlerT site IO) Html
genericIndex = basicSimpleCrudIndex (toWidget . toHtml . gcrudName . entityVal)

genericNamedOpts :: (PersistCrudEntity site c, HasName c) => HandlerT site IO (OptionList (Key c))
genericNamedOpts = optionsPersistKey [] [Asc gcrudNameField] gcrudName

