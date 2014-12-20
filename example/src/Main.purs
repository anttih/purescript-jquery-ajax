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
import JQuery.Ajax

data Person = Person String String Number

instance foreignPerson :: IsForeign Person where
  read value = do
    first <- readProp "firstname" value
    last <- readProp "lastname" value
    age <- readProp "age" value
    return (Person first last age)

instance showPerson :: Show Person where
  show (Person first last _) = "Person: " ++ first ++ " " ++ last

getPerson :: forall eff. EC eff (F Person)
getPerson = getJson "/public/person.json"

showTrace :: forall eff a. (Show a) => a -> Eff (trace :: Trace | eff) Unit
showTrace = show >>> trace

main = runContT (runErrorT getPerson) $ either showTrace handleOk
  where handleOk = either showTrace showTrace
