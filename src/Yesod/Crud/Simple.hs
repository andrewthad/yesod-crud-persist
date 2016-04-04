module Yesod.Crud.Simple where

import Prelude
import Data.Monoid
import Lens.Micro
import Lens.Micro.TH

import Yesod.Core
import Yesod.Form
import Yesod.Persist
import Database.Persist.Sql
import Data.Text (Text)
import Control.Monad
import Data.Proxy

import Yesod.Crud

data RedirectEdit = RedirectEditToView | RedirectEditToIndex

data SimpleCrud master p c = SimpleCrud
  { _scAdd        :: WidgetT master IO () -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _scIndex      :: p -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _scView       :: Key c -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _scEdit       :: WidgetT master IO () -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _scDelete     :: WidgetT master IO () -> HandlerT (Crud master p c) (HandlerT master IO) Html
  , _scDeleteForm :: WidgetT master IO () 
  , _scForm       :: Either p c -> Html -> MForm (HandlerT master IO) (FormResult c, WidgetT master IO ())
  , _scFormWrap   :: Enctype -> Route master -> WidgetT master IO () -> WidgetT master IO ()
  , _scDeleteDb   :: Key c -> YesodDB master p
  , _scAddDb      :: p -> c -> YesodDB master (Key c)
  , _scEditDb     :: Key c -> c -> YesodDB master p
  , _scMessageWrap  :: Html -> Html
  , _scRedirectEdit :: RedirectEdit
  }
makeLenses ''SimpleCrud

emptyParentlessSimpleCrud :: 
     PathPiece (Key a) 
  => Yesod master
  => YesodPersist master
  => PersistEntity a
  => PersistQuery (YesodPersistBackend master)
  => PersistEntityBackend a ~ YesodPersistBackend master
  => SimpleCrud master () a
emptyParentlessSimpleCrud = SimpleCrud 
  (const $ return mempty)  -- add 
  (const $ return mempty)  -- index
  (const $ return mempty)  -- view
  (const $ return mempty)  -- edit
  (const $ return mempty)  -- delete
  mempty (const $ const $ return (FormMissing,mempty)) -- delete form
  (const $ const $ const mempty) -- form wrapper
  delete -- default deletion, assumes no FK constraints
  (const insert) -- default DB add
  replace -- default DB edit
  id -- default message wrap
  RedirectEditToIndex

emptyChildSimpleCrud :: 
     PathPiece (Key a) 
  => Yesod master
  => YesodPersist master
  => PersistEntity a
  => PersistQuery (YesodPersistBackend master)
  => PersistEntityBackend a ~ YesodPersistBackend master
  => (Key a -> YesodDB master p) -> SimpleCrud master p a
emptyChildSimpleCrud getParent = SimpleCrud 
  (const $ return mempty)  -- add 
  (const $ return mempty)  -- index
  (const $ return mempty)  -- view
  (const $ return mempty)  -- edit
  (const $ return mempty)  -- delete
  mempty (const $ const $ return (FormMissing,mempty)) -- delete form
  (const $ const $ const mempty) -- form wrapper
  del -- default deletion, assumes no FK constraints
  (const insert) -- default DB add
  edit -- default DB edit
  id -- default message wrap
  RedirectEditToIndex
  where 
  del k = do
    p <- getParent k
    delete k
    return p
  edit k v = do
    replace k v
    getParent k

emptyHierarchySimpleCrud :: forall a c master.
     SqlBackend ~ YesodPersistBackend master
  => PersistStore (YesodPersistBackend master)
  => YesodPersist master
  => SqlClosure a c
  => SimpleCrud master (Maybe (Key a)) a 
emptyHierarchySimpleCrud = SimpleCrud
  (const $ return mempty)  -- add 
  (const $ return mempty)  -- index
  (const $ return mempty)  -- view
  (const $ return mempty)  -- edit
  (const $ return mempty)  -- delete
  mempty (const $ const $ return (FormMissing,mempty)) -- delete form
  (const $ const $ const mempty) -- form wrapper
  del -- deletion
  closureInsert -- default DB add
  edit -- default DB edit
  id -- default message wrap
  RedirectEditToIndex
  where 
  del k = closureGetParentIdProxied (Proxy :: Proxy c) k
  edit k v = do
    replace k v
    closureGetParentIdProxied (Proxy :: Proxy c) k

applyBasicLayoutsAndForms :: PersistCrudEntity master a
  => SimpleCrud master p a -> SimpleCrud master p a
applyBasicLayoutsAndForms initial = initial
  & scIndex      .~ basicSimpleCrudIndex (toWidget . toHtml . toPathPiece . entityKey)
  & scAdd        .~ lift . defaultLayout
  & scEdit       .~ lift . defaultLayout
  & scDelete     .~ lift . defaultLayout
  & scDeleteForm .~ [whamlet|<button type="submit">Delete|]
  & scFormWrap   .~ formWrap
  where formWrap enctype route inner = [whamlet|$newline never
          <form action="@{route}" enctype="#{enctype}" method="post">
            ^{inner}
        |]

basicSimpleCrudIndex :: (PersistCrudEntity site c)
  => (Entity c -> WidgetT site IO ()) -> p -> HandlerT (Crud site p c) (HandlerT site IO) Html
basicSimpleCrudIndex nameFunc p = do
  tp <- getRouteToParent
  cs <- lift $ runDB $ selectList [] []
  lift $ defaultLayout $ [whamlet|$newline never
    <h1>Index
    <p>
      <a href="@{tp (AddR p)}">Add
    <table.table>
      <thead>
        <tr>
          <th>ID
          <th>Edit
          <th>Delete
      <tbody>
        $forall c <- cs
          <tr>
            <td>^{nameFunc c}
            <td>
              <a href="@{tp (EditR (entityKey c))}">Edit
            <td>
              <a href="@{tp (DeleteR (entityKey c))}">Delete
  |]



basicSimpleCrud :: PersistCrudEntity master a => SimpleCrud master () a
basicSimpleCrud = applyBasicLayoutsAndForms emptyParentlessSimpleCrud

basicChildSimpleCrud :: PersistCrudEntity master a => (Key a -> YesodDB master p) -> SimpleCrud master p a
basicChildSimpleCrud f = applyBasicLayoutsAndForms (emptyChildSimpleCrud f)

basicHierarchySimpleCrud :: (PersistCrudEntity master a, SqlClosure a c)
  => SimpleCrud master (Maybe (Key a)) a
basicHierarchySimpleCrud = applyBasicLayoutsAndForms emptyHierarchySimpleCrud

simpleCrudToCrud :: 
     PersistEntityBackend a ~ YesodPersistBackend master
  => PersistEntity a
  => PersistStore (YesodPersistBackend master)
  => YesodPersist master
  => RenderMessage master FormMessage
  => SimpleCrud master p a -> Crud master p a
simpleCrudToCrud (SimpleCrud add index view edit del delForm form wrap delDb addDb editDb messageWrap redirectEdit) = 
  Crud addH indexH editH delH viewH
  where 
  indexH = index
  viewH = view
  delH theId = do
    tp <- getRouteToParent
    lift $ do
      res <- runInputPostResult $ ireq textField "fake"
      case res of
        FormSuccess _ -> do
          p <- runDB $ delDb theId
          setMessage $ messageWrap "You have deleted the resource."
          redirect (tp $ IndexR p)
        _ -> return ()
    del (wrap UrlEncoded (tp $ DeleteR theId) ([whamlet|<input type="hidden" value="a" name="fake">|] <> delForm))
  addH p = do 
    tp <- getRouteToParent
    (enctype,w) <- lift $ do
      ((res,w),enctype) <- runFormPost (form $ Left p)
      case res of
        FormSuccess a -> do
          void $ runDB $ addDb p a 
          setMessage $ messageWrap "You have created a new resource"
          redirect (tp $ IndexR p)
        _ -> return (enctype,w)
    add (wrap enctype (tp $ AddR p) w)
  editH theId = do
    tp <- getRouteToParent
    (enctype,w) <- lift $ do
      old <- runDB $ get404 theId
      ((res,w),enctype) <- runFormPost (form $ Right old)
      case res of
        FormSuccess new -> do
          p <- runDB $ editDb theId new
          setMessage $ messageWrap "You have updated the resource."
          redirect $ tp $ case redirectEdit of
            RedirectEditToView  -> ViewR theId
            RedirectEditToIndex -> IndexR p
        _ -> return (enctype,w)
    edit (wrap enctype (tp $ EditR theId) w)

