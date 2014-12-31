module JQuery.Ajax
  ( Ajax()
  , C()
  , ErrCont(..)
  , Header(..)
  , DataType(..)
  , Method(..)
  , URL()
  , Response(..)
  , JQueryAjaxOptions()
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
import Data.Options (Options(), Option(), IsOption, optionFn, options, (:=))

foreign import data Ajax :: !

type C eff = ContT Unit (Eff (ajax :: Ajax | eff))

type ErrCont eff = ErrorT Response (C eff)

newtype Response = Response {status :: Number, responseText :: String}

instance errorResponse :: Error Response where
  noMsg = makeResponse 0 ""
  strMsg = makeResponse 0

instance showResponse :: Show Response where
  show (Response err) = (show $ err.status) ++ " " ++ err.responseText

makeResponse :: Number -> String -> Response
makeResponse status text = Response {status: status, responseText: text}

type URL = String

data Header = Header String String

type AjaxResponse = {status :: Number, responseText :: String}

foreign import jqueryAjaxImpl
  """
  function jqueryAjaxImpl(settings, onError, onSuccess) {
    return function() {
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
  """ :: forall eff. Fn3 Foreign
                         (AjaxResponse -> Eff (ajax :: Ajax | eff) Unit)
                         (Foreign -> Eff (ajax :: Ajax | eff) Unit)
                         (Eff (ajax :: Ajax | eff) Unit)

jqueryAjax :: forall eff. Options JQueryAjaxOptions
                       -> (Either Response Foreign -> Eff (ajax :: Ajax | eff) Unit)
                       -> Eff (ajax :: Ajax | eff) Unit
jqueryAjax s cb = runFn3 jqueryAjaxImpl (options s) (cb <<< Left <<< Response) (cb <<< Right)

jqueryAjaxCont :: forall eff. Options JQueryAjaxOptions -> ErrCont eff Foreign
jqueryAjaxCont s = ErrorT $ ContT $ jqueryAjax s

getWith :: forall eff. Options JQueryAjaxOptions -> URL -> ErrCont eff Foreign
getWith opts url' = jqueryAjaxCont $ url := url' <> opts

getJson :: forall eff a. (IsForeign a) => URL -> ErrCont eff (F a)
getJson url' = read <$> getWith (dataType := JSON) url'

get :: forall eff. URL -> ErrCont eff Foreign
get url' = jqueryAjaxCont $ url := url'

post :: forall eff. URL -> ErrCont eff Foreign
post url' = jqueryAjaxCont (method := POST <> url := url')

put :: forall eff. URL -> ErrCont eff Foreign
put url' = jqueryAjaxCont (method := PUT <> url := url')

delete :: forall eff. URL -> ErrCont eff Foreign
delete url' = jqueryAjaxCont $ (method := DELETE <> url := url')

-- options
foreign import data JQueryAjaxOptions :: *

foreign import unsafeToOption
  """
  function unsafeToOption(s) {
    return s;
  }
  """ :: forall a. String -> Option JQueryAjaxOptions a

url :: Option JQueryAjaxOptions String
url = unsafeToOption "url"

data Method = GET | POST | PUT | DELETE

instance showMethod :: Show Method where
  show GET = "get"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"

instance methodIsOption :: IsOption Method where
  (:=) k m = (optionFn k) := show m

method :: Option JQueryAjaxOptions Method
method = unsafeToOption "method"

data DataType = Text | JSON | HTML

instance showDataType :: Show DataType where
  show Text = "text"
  show JSON = "json"
  show HTML = "html"

instance dataTypeIsOption :: IsOption DataType where
  (:=) k a = (optionFn k) := show a

dataType :: Option JQueryAjaxOptions DataType
dataType = unsafeToOption "dataType"
