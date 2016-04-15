# yesod-crud-persist

This is a library for making it easier to work with CRUD resources in haskell.
It does not use yesod subsites because after many months working with it, I
found that a `PathMultiPiece` approach ended up making it easier to use at 
no cost to its capabilities.

The two most important types are `CrudHandler` and `CrudRoute`. They are
defined as follows:

    data CrudHandler site p c = CrudHandler
      { _chAdd    :: p -> HandlerT site IO Html
      , _chIndex  :: p -> HandlerT site IO Html
      , _chEdit   :: Key c -> HandlerT site IO Html
      , _chDelete :: Key c -> HandlerT site IO Html
      , _chView   :: Key c -> HandlerT site IO Html
      }
    
    data CrudRoute p c
      = AddR p
      | IndexR p
      | EditR (Key c)
      | DeleteR (Key c)
      | ViewR (Key c)

In these data type, the type variables should be read as:

- `site`: Your site type, probably `App`
- `p`: The parent resources ID. This will sometimes be a database key (like `PersonId`).
  It will sometimes be unit (written as `()` in haskell)
- `c`: The type of the child resource (not its ID type)

As an example, let's say we have two types, `Person` and `Dog`. Each `Person` has many
`Dog`s. Let's assume that we want a site map that looks like this:

- Person Index
  - Person View
    - Person Edit
    - Person Delete
    - Dog Index (dogs belonging to this person)
      - Dog View
      - Dog Edit
      - Dog Delete

This is what you would need to write to get this. First you will need to add a type
synonym that will let you refer to `()` as `Unit` (`yesod`'s TH routing doesn't
like types with symbols).

    type Unit = ()

Now, the routes:

    /person/*CrudRoute-Unit-Person PersonR
    /dog/*CrudRoute-PersonId-Dog   DogR

So, yesod's dispatch TH is going to require that we define
functions named `handlePersonR` and `handleDogR` somewhere. Let's
define `handlePersonR` using the `crudHandler` function (which is
defined in `Yesod.Crud`):

    handlePersonR :: CrudRoute () Person -> Handler Html
    handlePersonR = handleCrud personCrudHandler
    
    personCrudHandler :: CrudHandler App () Person
    personCrudHandler = ...

And you can trivially fill in the five fields for a `CrudHandler`.
If you did this for `handleDogR` as well, everything would work.

That is the simplistic way to use this library. The only thing we
gained in the above scenario was that we only had to write out
two routes instead of ten in `config/routes`. (Technically, if 
you're using yesod's breadcrumbs features, this also allows you
to cut down on boilerplate there as well). That's a pretty
small benefit.

The other feature this library offers is in `Yesod.Crud.Simple`.
A lot of the time, when you are writing your add and edit handlers,
a really common pattern emerges. This module capitalizes on that
pattern. It also has a really bad looking default for an index page
and a view page. I'll try to write more about this later, but here
are some samples from code I've worked on:

    import Yesod.Bootstrap -- from yesod-bootstrap

    myParentlessSimpleCrud :: PersistCrudEntity App c
      => (CrudRoute () c -> Route App) -> Text -> SimpleCrud App () c
    myParentlessSimpleCrud tp name = basicSimpleCrud tp
      & scEdit .~ (\w -> layout $ do
        h1_ [] $ tw $ "Edit " <> name
        w)
      & scAdd .~ (\w -> layout $ do
        h1_ [] $ tw $ "Add " <> name
        w)
      & scDelete .~ (\w -> layout $ do
        h1_ [] $ tw $ "Delete " <> name
        w)
      & scEditParent  .~ EditParentView
      & scMessageWrap .~ alertHtml Info
      & scDeleteForm  .~ [whamlet|<button.btn.btn-danger>Delete|]

    handleCustomerR :: CrudRoute () Customer -> Handler Html
    handleCustomerR = handleCrud $ toCrudHandler 
      $ myParentlessSimpleCrud CustomerR name
        & scForm  .~ form
        & scIndex .~ const index -- index defined elsewhere
        & scView  .~ viewer      -- viewer defined elsewhere
      where
      form e = let m = rightToMaybe e in renderForm $ Customer
        <$> areq textField (bft "Name") (customerName <$> m)
        <*> areq (selectField allAddressOpts) (bft "Shipping Address") (customerShipping <$> m)
        <*> areq (selectField allAddressOpts) (bft "Billing Address") (customerBilling <$> m)
        <*> atextArea 5 (bft "Notes") (customerNotes <$> m)
        <*> areq (selectField groupOpts) (bft "Customer Group") (customerCustogroup <$> m)
        <*  primarySubmit "Submit"
      groupOpts = optionsPersistKey [] [Asc CustogroupName] custogroupName

This is just a sample that is pulled out of the context of a larger
project with models and field helpers, so it won't compile, but
maybe it will provide an idea of how to use `Yesod.Crud.Simple`.

