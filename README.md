# Module Documentation

## Module JQuery.Ajax

### Types

    data Ajax :: !

    type C eff = ContT Unit (Eff (ajax :: Ajax | eff))

    data DataType where
      Text :: DataType
      JSON :: DataType
      HTML :: DataType

    type ErrCont eff = ErrorT Response (C eff)

    data Header where
      Header :: String -> String -> Header

    data JQueryAjaxOptions :: *

    data Method where
      GET :: Method
      POST :: Method
      PUT :: Method
      DELETE :: Method

    newtype Response where
      Response :: { responseText :: String, status :: Number } -> Response

    type URL = String


### Type Class Instances

    instance dataTypeIsOption :: IsOption DataType

    instance errorResponse :: Error Response

    instance methodIsOption :: IsOption Method

    instance showDataType :: Show DataType

    instance showMethod :: Show Method

    instance showResponse :: Show Response


### Values

    body :: forall attrs. Option JQueryAjaxOptions String

    contentType :: Option JQueryAjaxOptions String

    dataType :: Option JQueryAjaxOptions DataType

    delete :: forall eff. URL -> ErrCont eff Foreign

    get :: forall eff. URL -> ErrCont eff Foreign

    getJSON :: forall eff a. (IsForeign a) => URL -> ErrCont eff (F a)

    getWith :: forall eff. Options JQueryAjaxOptions -> URL -> ErrCont eff Foreign

    method :: Option JQueryAjaxOptions Method

    postWith :: forall eff. URL -> Options JQueryAjaxOptions -> ErrCont eff Foreign

    putWith :: forall eff. URL -> Options JQueryAjaxOptions -> ErrCont eff Foreign

    url :: Option JQueryAjaxOptions String



