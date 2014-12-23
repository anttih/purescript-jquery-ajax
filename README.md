# Module Documentation

## Module JQuery.Ajax

### Types

    data Ajax :: !

    data DataType where
      Text :: DataType
      JSON :: DataType
      HTML :: DataType

    type ErrCont eff = ErrorT Response (C eff)

    data Header where
      Header :: String -> String -> Header

    data Method where
      GET :: Method
      POST :: Method
      PUT :: Method
      DELETE :: Method

    newtype Response where
      Response :: { responseText :: String, status :: Number } -> Response

    type Settings = { dataType :: DataType, headers :: [Header], method :: Method, url :: URL }

    type URL = String


### Type Class Instances

    instance errorResponse :: Error Response

    instance showResponse :: Show Response


### Values

    dataType :: DataType -> Settings -> Settings

    delete :: forall eff. URL -> ErrCont eff Foreign

    get :: forall eff. URL -> ErrCont eff Foreign

    getJson :: forall eff a. (IsForeign a) => URL -> ErrCont eff (F a)

    getWith :: forall eff. (Settings -> Settings) -> URL -> ErrCont eff Foreign

    method :: Method -> Settings -> Settings

    post :: forall eff. URL -> ErrCont eff Foreign

    put :: forall eff. URL -> ErrCont eff Foreign

    url :: String -> Settings -> Settings



