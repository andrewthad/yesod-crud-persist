module Yesod.Crud.Simple where

import Prelude
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Lens.TH
import Control.Lens

import Yesod.Core
import Yesod.Form
import Yesod.Persist
import Data.Text (Text)
import Database.Persist hiding (get)

import Yesod.Crud

data SimpleCrud master a = SimpleCrud
  { _scAdd        :: WidgetT master IO () -> HandlerT (Crud master a) (HandlerT master IO) Html
  , _scIndex      :: HandlerT (Crud master a) (HandlerT master IO) Html
  , _scEdit       :: WidgetT master IO () -> HandlerT (Crud master a) (HandlerT master IO) Html
  , _scDelete     :: WidgetT master IO () -> HandlerT (Crud master a) (HandlerT master IO) Html
  , _scDeleteForm :: WidgetT master IO () 
  , _scForm       :: Maybe a -> Html -> MForm (HandlerT master IO) (FormResult a, WidgetT master IO ())
  , _scFormWrap   :: Enctype -> Route master -> WidgetT master IO () -> WidgetT master IO ()
  }
makeLenses ''SimpleCrud

emptySimpleCrud :: SimpleCrud master a
emptySimpleCrud = SimpleCrud (const $ return mempty) (return mempty) (const $ return mempty) (const $ return mempty) 
                  mempty (const $ const $ return (FormMissing,mempty)) (const $ const $ const mempty) 

basicSimpleCrud :: forall master a. 
     PathPiece (Key a) 
  => Yesod master
  => YesodPersist master
  => PersistEntity a
  => PersistQuery (YesodPersistBackend master)
  => PersistEntityBackend a ~ YesodPersistBackend master
  => SimpleCrud master a
basicSimpleCrud = emptySimpleCrud
  & scIndex      .~ index
  & scAdd        .~ lift . defaultLayout
  & scEdit       .~ lift . defaultLayout
  & scDelete     .~ lift . defaultLayout
  & scDeleteForm .~ [whamlet|<button type="submit">Delete|]
  & scFormWrap   .~ formWrap
  where formWrap enctype route inner = [whamlet|$newline never
          <form action="@{route}" enctype="#{enctype}" method="post">
            ^{inner}
        |]
        index :: HandlerT (Crud master a) (HandlerT master IO) Html 
        index = do
          tp <- getRouteToParent
          as <- lift $ runDB $ selectList [] []
          let _ = as :: [Entity a]
          lift $ defaultLayout $ [whamlet|$newline never
            <h1>Index
            <p>
              <a href="@{tp AddR}">Add
            <table>
              <thead>
                <tr>
                  <th>ID
                  <th>Edit
                  <th>Delete
              <tbody>
                $forall (Entity theId _) <- as
                  <tr>
                    <td>#{toPathPiece theId}
                    <td>
                      <a href="@{tp (EditR theId)}">Edit
                    <td>
                      <a href="@{tp (DeleteR theId)}">Delete
          |]

simpleCrudToCrud :: PersistEntityBackend a ~ YesodPersistBackend master
  => PersistEntity a
  => PersistStore (YesodPersistBackend master)
  => YesodPersist master
  => RenderMessage master FormMessage
  => SimpleCrud master a -> Crud master a
simpleCrudToCrud (SimpleCrud add index edit del delForm form wrap) = 
  Crud addH indexH editH delH
  where 
  indexH = index
  delH theId = do
    tp <- getRouteToParent
    lift $ do
      res <- runInputPostResult $ ireq textField "fake"
      case res of
        FormSuccess _ -> do
          runDB $ delete theId
          setMessageI ("You have deleted the resource." :: Text)
          redirect (tp IndexR)
        _ -> return ()
    del (wrap UrlEncoded (tp $ DeleteR theId) ([whamlet|<input type="hidden" value="a" name="fake">|] <> delForm))
  addH = do 
    tp <- getRouteToParent
    (enctype,w) <- lift $ do
      ((res,w),enctype) <- runFormPost (form Nothing)
      case res of
        FormSuccess a -> do
          runDB $ insert_ a 
          setMessageI ("You have created a new resource." :: Text)
          redirect (tp IndexR)
        _ -> return (enctype,w)
    add (wrap enctype (tp AddR) w)
  editH theId = do
    tp <- getRouteToParent
    (enctype,w) <- lift $ do
      old <- runDB $ get404 theId
      ((res,w),enctype) <- runFormPost (form $ Just old)
      case res of
        FormSuccess new -> do
          runDB $ replace theId new
          setMessageI ("You have updated the resource." :: Text)
          redirect (tp IndexR)
        _ -> return (enctype,w)
    edit (wrap enctype (tp $ EditR theId) w)

