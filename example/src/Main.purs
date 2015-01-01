module Main where

import Control.Monad.Eff
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Debug.Trace
import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.Cont.Trans
import Control.Monad.Trans
import Data.JSON (ToJSON, object, (.=), encode)
import Data.Options (Options(), (:=))
import JQuery.Ajax

newtype Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Number
  }

foreign import showPersonImpl
  """
  function showPersonImpl(p) {
    return JSON.stringify(p);
  }
  """ :: forall a. a -> String

instance showPerson :: Show Person where
  show (Person p) = showPersonImpl p

instance toJsonPerson :: ToJSON Person where
  toJSON (Person {firstName = first, lastName = last, age = age}) = 
    object ["firstName" .= first, "lastName" .= last, "age" .= age]

contentTypeJson :: Options JQueryAjaxOptions
contentTypeJson = contentType := "application/json; charset=utf-8"

postJSON :: forall eff a. (ToJSON a) => a -> URL -> ErrCont eff Foreign
postJSON d url' = postWith url' $ setData := (encode d) <> contentTypeJson

instance foreignPerson :: IsForeign Person where
  read value = do
    first <- readProp "firstname" value
    last <- readProp "lastname" value
    age <- readProp "age" value
    return $ Person {firstName: first, lastName: last, age: age}

getPerson :: forall eff. ErrCont eff (F Person)
getPerson = do
  postJSON (Person {firstName: "Antti", lastName: "Holvikari", age: 30}) "/public/person.json"
  getJSON "/public/person.json"

showTrace :: forall eff a. (Show a) => a -> Eff (trace :: Trace | eff) Unit
showTrace = show >>> trace

main = runContT (runErrorT getPerson) $ either showTrace handleOk
  where handleOk = either showTrace showTrace
