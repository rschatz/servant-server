{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | This module lets you implement 'Server's for defined APIs. You'll
-- most likely just need 'serve'.
module Servant.Server
  ( -- * Implementing an API
    serve,
    serveT

  , -- * Handlers for all standard combinators
    HasServer(..),
    Server
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Proxy (Proxy)
import Network.Wai (Application)

import Servant.Server.Internal


-- * Implementing Servers

-- | 'serve' allows you to implement an API and produce a wai 'Application'.
--
-- Example:
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > app :: Application
-- > app = serve myApi server
-- >
-- > main :: IO ()
-- > main = Network.Wai.Handler.Warp.run 8080 app
serve :: (HasServer layout m, m ~ IO) => Proxy layout -> Server layout -> Application
serve = serveT id

serveT :: (HasServer layout m, MonadIO m) => (forall a. m a -> IO a) -> Proxy layout -> ServerT layout m -> Application
serveT run p server = toApplication (route p server) run
