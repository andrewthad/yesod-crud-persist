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

data SimpleCrud site p c = SimpleCrud
  { _scAdd          :: WidgetT site IO () -> HandlerT site IO Html
  , _scIndex        :: p -> HandlerT site IO Html
  , _scView         :: Key c -> HandlerT site IO Html
  , _scEdit         :: WidgetT site IO () -> HandlerT site IO Html
  , _scDelete       :: WidgetT site IO () -> HandlerT site IO Html
  , _scDeleteForm   :: WidgetT site IO () 
  , _scForm         :: Either p c -> Html -> MForm (HandlerT site IO) (FormResult c, WidgetT site IO ())
  , _scFormWrap     :: Enctype -> Route site -> WidgetT site IO () -> WidgetT site IO ()
  , _scDeleteDb     :: Key c -> YesodDB site p
  , _scAddDb        :: p -> c -> YesodDB site (Key c)
  , _scEditDb       :: Key c -> c -> YesodDB site p
  , _scMessageWrap  :: Html -> Html
  , _scEditParent   :: EditParent
  , _scPromoteRoute :: CrudRoute p c -> Route site
  }
makeLenses ''SimpleCrud

emptyParentlessSimpleCrud :: PersistCrudEntity site c
  => (CrudRoute () c -> Route site) -> SimpleCrud site () c
emptyParentlessSimpleCrud tp = SimpleCrud 
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
  EditParentIndex
  tp

emptyChildSimpleCrud :: PersistCrudEntity site c
  => (CrudRoute p c -> Route site) -> (Key c -> YesodDB site p) -> SimpleCrud site p c
emptyChildSimpleCrud tp getParent = SimpleCrud 
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
  EditParentIndex
  tp 
  where 
  del k = do
    p <- getParent k
    delete k
    return p
  edit k v = do
    replace k v
    getParent k

applyBasicLayoutsAndForms :: PersistCrudEntity site a
  => SimpleCrud site p a -> SimpleCrud site p a
applyBasicLayoutsAndForms initial = initial
  & scIndex      .~ basicSimpleCrudIndex (_scPromoteRoute initial) (toWidget . toHtml . toPathPiece . entityKey)
  & scAdd        .~ defaultLayout
  & scEdit       .~ defaultLayout
  & scDelete     .~ defaultLayout
  & scDeleteForm .~ [whamlet|<button type="submit">Delete|]
  & scFormWrap   .~ formWrap
  where formWrap enctype route inner = [whamlet|$newline never
          <form action="@{route}" enctype="#{enctype}" method="post">
            ^{inner}
        |]

basicSimpleCrudIndex :: (PersistCrudEntity site c)
  => (CrudRoute p c -> Route site) -> (Entity c -> WidgetT site IO ()) -> p -> HandlerT site IO Html
basicSimpleCrudIndex tp nameFunc p = do
  cs <- runDB $ selectList [] []
  defaultLayout $ [whamlet|$newline never
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

basicSimpleCrud :: PersistCrudEntity site c => (CrudRoute () c -> Route site) -> SimpleCrud site () c
basicSimpleCrud tp = applyBasicLayoutsAndForms (emptyParentlessSimpleCrud tp)

basicChildSimpleCrud :: PersistCrudEntity site c => (CrudRoute p c -> Route site) -> (Key c -> YesodDB site p) -> SimpleCrud site p c
basicChildSimpleCrud tp f = applyBasicLayoutsAndForms (emptyChildSimpleCrud tp f)

simpleCrudToCrud :: (PersistCrudEntity site c, RenderMessage site FormMessage) 
  => SimpleCrud site p c -> CrudHandler site p c
simpleCrudToCrud (SimpleCrud add index view edit del delForm form wrap delDb addDb editDb messageWrap editParent tp) = 
  CrudHandler addH indexH editH delH viewH
  where 
  indexH = index
  viewH = view
  delH theId = do
    res <- runInputPostResult $ ireq textField "fake"
    case res of
      FormSuccess _ -> do
        p <- runDB $ delDb theId
        setMessage $ messageWrap "You have deleted the resource."
        redirect (tp $ IndexR p)
      _ -> return ()
    del (wrap UrlEncoded (tp $ DeleteR theId) ([whamlet|<input type="hidden" value="a" name="fake">|] <> delForm))
  addH p = do 
    (enctype,w) <- do
      ((res,w),enctype) <- runFormPost (form $ Left p)
      case res of
        FormSuccess a -> do
          void $ runDB $ addDb p a 
          setMessage $ messageWrap "You have created a new resource"
          redirect (tp $ IndexR p)
        _ -> return (enctype,w)
    add (wrap enctype (tp $ AddR p) w)
  editH theId = do
    (enctype,w) <- do
      old <- runDB $ get404 theId
      ((res,w),enctype) <- runFormPost (form $ Right old)
      case res of
        FormSuccess new -> do
          p <- runDB $ editDb theId new
          setMessage $ messageWrap "You have updated the resource."
          redirect $ tp $ case editParent of
            EditParentView  -> ViewR theId
            EditParentIndex -> IndexR p
        _ -> return (enctype,w)
    edit (wrap enctype (tp $ EditR theId) w)

