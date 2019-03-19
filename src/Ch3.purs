module Ch3 where

import Prelude 

import Data.AddressBook (AddressBook, Entry)
import Data.List (null, filter, head, nubBy)
import Data.Maybe (Maybe)


findStreet :: String -> AddressBook -> Maybe Entry
findStreet street =
    filter filterEntry >>> head 
  where 
    filterEntry :: Entry -> Boolean
    filterEntry e = e.address.street == street

hasName :: String -> String -> AddressBook -> Boolean 
hasName firstName lastName =
    filter filterEntry >>> null
  where filterEntry e = e.firstName == firstName && e.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook 
removeDuplicates = nubBy isSame 
  where
    isSame x y = x.firstName == y.firstName && x.lastName == y.lastName
