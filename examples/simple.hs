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
import SumTypeExample
import PlainDataExample


data Address = Address { addressStreetNr :: Int
                       , addressStreetName :: Text
                       } deriving (Show, Eq)
$(deriveSerializedEntity ''Address)


data User = User { userFirstName :: Text
                 , userLastName  :: Text
                 , userFavNr     :: Int
                 , userAddress   :: Address
                 } deriving(Show)
$(deriveSerializedEntity ''User)


insertUsers :: MonadIO m => MongoDB.Action m ()
insertUsers = insertMany
  [ User { userFirstName = "deian"
         , userLastName = "stefan"
         , userFavNr = 3
         , userAddress = Address
             { addressStreetNr = 123
             , addressStreetName = "Mission"
             }
         }

  , User { userFirstName = "amit"
         , userLastName = "levy"
         , userFavNr = 42
         , userAddress = Address
             { addressStreetNr = 42
             , addressStreetName = "Market"
             }
         }

  , User { userFirstName = "david"
         , userLastName = "mazieres"
         , userFavNr = 1337
         , userAddress = Address
             { addressStreetNr = 821
             , addressStreetName = "Valencia"
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
