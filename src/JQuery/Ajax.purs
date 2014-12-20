module JQuery.Ajax
  ( Ajax()
  , EC()
  , Cb()
  , Settings()
  , Header(..)
  , DataType(..)
  , Method(..)
  , URL()
  , Response(..)
  , getWith
  , get
  , getJson
  , put
  , post
  , delete
  , url
  , method
  , dataType
  ) where

import Control.Monad.Eff
import Data.Either
import Control.Monad.Error
import Control.Monad.Cont.Trans
import Control.Monad.Error.Trans
import Data.Foreign.Class
import Data.Foreign
import Data.Function

foreign import data Ajax :: !

type Cb eff = ContT Unit (Eff (ajax :: Ajax | eff))

type EC eff = ErrorT Response (Cb eff)

newtype Response = Response {status :: Number, responseText :: String}

instance errorResponse :: Error Response where
  noMsg = makeResponse 0 ""
  strMsg = makeResponse 0

instance showResponse :: Show Response where
  show (Response err) = (show $ err.status) ++ " " ++ err.responseText

makeResponse :: Number -> String -> Response
makeResponse status text = Response {status: status, responseText: text}

data Method = GET | POST | PUT | DELETE

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"

type URL = String

data DataType = Text | JSON | HTML

data Header = Header String String

type Settings =
  { url :: URL
  , method :: Method
  , headers :: [Header]
  , dataType :: DataType
  }

type AjaxResponse = {status :: Number, responseText :: String}

foreign import jqueryAjaxImpl
  """
  function jqueryAjaxImpl(settings, onError, onSuccess) {
    return function() {
      settings.method = settings.method.constructor.name;
      settings.dataType = settings.dataType.constructor.name.toLowerCase();
      return jQuery.ajax(jQuery.extend({}, settings, {
        success: function(data) {
          onSuccess(data)();
        },
        error: function(error) {
          onError(error)();
        }
      }));
    }
  }
  """ :: forall eff. Fn3 Settings
                         (AjaxResponse -> Eff (ajax :: Ajax | eff) Unit)
                         (Foreign -> Eff (ajax :: Ajax | eff) Unit)
                         (Eff (ajax :: Ajax | eff) Unit)

jqueryAjax :: forall eff. Settings
                       -> (Either Response Foreign -> Eff (ajax :: Ajax | eff) Unit)
                       -> Eff (ajax :: Ajax | eff) Unit
jqueryAjax s cb = runFn3 jqueryAjaxImpl s (cb <<< Left <<< Response) (cb <<< Right)

jqueryAjaxCont :: forall eff. Settings -> EC eff Foreign
jqueryAjaxCont s = ErrorT $ ContT $ jqueryAjax s

makeAjaxRequest :: forall eff. (Settings -> Settings) -> EC eff Foreign
makeAjaxRequest sf = jqueryAjaxCont $ sf defaultSettings

defaultSettings :: Settings
defaultSettings =
  { method: GET
  , url: "/"
  , headers: []
  , dataType: Text
  }

getWith :: forall eff. (Settings -> Settings) -> URL -> EC eff Foreign
getWith sf url' = makeAjaxRequest $ (url url' >>> sf)

getJson :: forall eff a. (IsForeign a) => URL -> EC eff (F a)
getJson url' = read <$> getWith (dataType JSON) url'

get :: forall eff. URL -> EC eff Foreign
get url' = makeAjaxRequest $ url url'

post :: forall eff. URL -> EC eff Foreign
post url' = makeAjaxRequest $ (method POST >>> url url')

put :: forall eff. URL -> EC eff Foreign
put url' = makeAjaxRequest $ (method PUT >>> url url')

delete :: forall eff. URL -> EC eff Foreign
delete url' = makeAjaxRequest $ (method DELETE >>> url url')

-- options
url :: String -> Settings -> Settings
url url' s = s { url = url' }

method :: Method -> Settings -> Settings
method m s = s { method = m }

dataType :: DataType -> Settings -> Settings
dataType t s = s { dataType = t }
