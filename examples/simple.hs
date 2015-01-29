{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class
import qualified Data.Bson as Bson
import Data.Text (Text)
import Database.MongoDB as MongoDB (Action)
import Database.MongoDB.Connection
import Control.Monad (forM_)

import Database.MongoDB (access, master)
import Database.MongoDB.Structured
import Database.MongoDB.Structured.TH


data Address = Address { streetNr :: Int
                       , streetName :: Text
                       } deriving (Show, Eq)
$(deriveStructured ''Address)


data User = User { firstName :: Text
                 , lastName  :: Text
                 , favNr     :: Int
                 , address   :: Address
                 } deriving(Show)
$(deriveStructured ''User)


insertUsers :: MonadIO m => MongoDB.Action m ()
insertUsers = insertMany 
  [ User { firstName = "deian"
         , lastName = "stefan"
         , favNr = 3
         , address = Address { streetNr = 123
                             , streetName = "Mission"
                             }
         }
  
  , User { firstName = "amit" 
         , lastName = "levy"
         , favNr = 42 
         , address = Address { streetNr = 42
                             , streetName = "Market"
                             }
         }
  
  , User { firstName = "david"
         , lastName = "mazieres"
         , favNr = 1337 
         , address = Address { streetNr = 821
                             , streetName = "Valencia"
                             }
         }
  ]

run :: MongoDB.Action IO ()
run = do
   delete ([] :: [Filter User])
   insertUsers

   liftIO $ putStrLn "Querying users"

   liftIO . print $ (select [UserAddress!.AddressStreetNr ->. [1, 2, 3]])

   let query = (select [UserAddress!.AddressStreetNr ==. 123 ||. UserFavNr >. 3]) { limit = 1, sort = [asc UserLastName, asc UserFirstName] }
   liftIO $ print query

   users <- find query >>= rest
   liftIO $ printFunc users

   liftIO $ putStrLn "Updating user"
   modify [UserFirstName ==. "david"] [UserFavNr =~ 137]

   find (select [UserFirstName ==. "david"]) >>= rest >>= liftIO . printFunc

    where printFunc users = forM_ users $ \u ->
            putStrLn . show $ (u :: Entity User)

main :: IO ()
main = do
   pipe <- connect (host "mongo")
   e <- access pipe master "auth" run
   close pipe
   print e
