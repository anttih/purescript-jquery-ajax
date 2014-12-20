# Module Documentation

## Module JQuery.Ajax

### Types

    data Ajax :: !

    type Cb eff = ContT Unit (Eff (ajax :: Ajax | eff))

    data DataType where
      Text :: DataType
      JSON :: DataType
      HTML :: DataType

    type EC eff = ErrorT Response (Cb eff)

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

    delete :: forall eff. URL -> EC eff Foreign

    get :: forall eff. URL -> EC eff Foreign

    getJson :: forall eff a. (IsForeign a) => URL -> EC eff (F a)

    getWith :: forall eff. (Settings -> Settings) -> URL -> EC eff Foreign

    method :: Method -> Settings -> Settings

    post :: forall eff. URL -> EC eff Foreign

    put :: forall eff. URL -> EC eff Foreign

    url :: String -> Settings -> Settings



