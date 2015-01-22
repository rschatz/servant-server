{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Servant.ServerSpec where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Control.Monad.Trans.State (State, evalState)
import qualified Control.Monad.Trans.State as State (modify, get)
import Data.Aeson (ToJSON, FromJSON, encode, decode')
import Data.Char (toUpper)
import Data.Proxy (Proxy(Proxy))
import Data.String (fromString)
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import Network.HTTP.Types (parseQuery, ok200)
import Network.Wai (Application, Request, responseLBS, pathInfo, queryString, rawQueryString)
import Network.Wai.Test (runSession, request, defaultRequest, simpleBody)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Wai (liftIO, with, get, post, shouldRespondWith, matchStatus)

import Servant.API.Capture (Capture)
import Servant.API.Get (Get)
import Servant.API.ReqBody (ReqBody)
import Servant.API.Post (Post)
import Servant.API.QueryParam (QueryParam, QueryParams, QueryFlag)
import Servant.API.MatrixParam (MatrixParam, MatrixParams, MatrixFlag)
import Servant.API.Raw (Raw)
import Servant.API.Sub ((:>))
import Servant.API.Alternative ((:<|>)((:<|>)))
import Servant.Server (Server, ServerT, serve, serveT, enter)


-- * test data types

data Person = Person {
  name :: String,
  age :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

alice :: Person
alice = Person "Alice" 42

data Animal = Animal {
  species :: String,
  numberOfLegs :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Animal
instance FromJSON Animal

jerry :: Animal
jerry = Animal "Mouse" 4

tweety :: Animal
tweety = Animal "Bird" 2


-- * specs

spec :: Spec
spec = do
  captureSpec
  getSpec
  queryParamSpec
  matrixParamSpec
  postSpec
  rawSpec
  unionSpec
  monadSpec
  enterSpec


type CaptureApi = Capture "legs" Integer :> Get Animal
captureApi :: Proxy CaptureApi
captureApi = Proxy
captureServer :: Integer -> EitherT (Int, String) IO Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> left (404, "not found")

captureSpec :: Spec
captureSpec = do
  describe "Servant.API.Capture" $ do
    with (return (serve captureApi captureServer)) $ do
      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just tweety

    with (return (serve
        (Proxy :: Proxy (Capture "captured" String :> Raw))
        (\ "captured" request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" `shouldRespondWith` (fromString (show ["foo" :: String]))


type GetApi = Get Person
getApi :: Proxy GetApi
getApi = Proxy

getSpec :: Spec
getSpec = do
  describe "Servant.API.Get" $ do
    with (return (serve getApi (return alice))) $ do
      it "allows to GET a Person" $ do
        response <- get "/"
        return response `shouldRespondWith` 200
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just alice

      it "throws 405 (wrong method) on POSTs" $ do
        post "/" "" `shouldRespondWith` 405


type QueryParamApi = QueryParam "name" String :> Get Person
                :<|> "a" :> QueryParams "names" String :> Get Person
                :<|> "b" :> QueryFlag "capitalize" :> Get Person

queryParamApi :: Proxy QueryParamApi
queryParamApi = Proxy

qpServer :: Server QueryParamApi
qpServer = queryParamServer :<|> qpNames :<|> qpCapitalize

  where qpNames (_:name2:_) = return alice { name = name2 }
        qpNames _           = return alice

        qpCapitalize False = return alice
        qpCapitalize True  = return alice { name = map toUpper (name alice) }

        queryParamServer (Just name_) = return alice{name = name_}
        queryParamServer Nothing = return alice

queryParamSpec :: Spec
queryParamSpec = do
  describe "Servant.API.QueryParam" $ do
      it "allows to retrieve simple GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params1 = "?name=bob"
          response1 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params1,
            queryString = parseQuery params1
           }
          liftIO $ do
            decode' (simpleBody response1) `shouldBe` Just alice{
              name = "bob"
             }

      it "allows to retrieve lists in GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params2 = "?names[]=bob&names[]=john"
          response2 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params2,
            queryString = parseQuery params2,
            pathInfo = ["a"]
           }
          liftIO $
            decode' (simpleBody response2) `shouldBe` Just alice{
              name = "john"
             }

      it "allows to retrieve value-less GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params3 = "?capitalize"
          response3 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3,
            queryString = parseQuery params3,
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3) `shouldBe` Just alice{
              name = "ALICE"
             }

          let params3' = "?capitalize="
          response3' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3',
            queryString = parseQuery params3',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "ALICE"
             }

          let params3'' = "?unknown="
          response3' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3'',
            queryString = parseQuery params3'',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "Alice"
             }

type MatrixParamApi = "a" :> MatrixParam "name" String :> Get Person
                :<|> "b" :> MatrixParams "names" String :> "bsub" :> MatrixParams "names" String :> Get Person
                :<|> "c" :> MatrixFlag "capitalize" :> Get Person
                :<|> "d" :> Capture "foo" Integer :> MatrixParam "name" String :> MatrixFlag "capitalize" :> "dsub" :> Get Person

matrixParamApi :: Proxy MatrixParamApi
matrixParamApi = Proxy

mpServer :: Server MatrixParamApi
mpServer = matrixParamServer :<|> mpNames :<|> mpCapitalize alice :<|> mpComplex
  where mpNames (_:name2:_) _ = return alice { name = name2 }
        mpNames _           _ = return alice

        mpCapitalize p False = return p
        mpCapitalize p True  = return p { name = map toUpper (name p) }

        matrixParamServer (Just name) = return alice{name = name}
        matrixParamServer Nothing = return alice

        mpAge age p = return p { age = age }
        mpComplex capture name cap = matrixParamServer name >>= flip mpCapitalize cap >>= mpAge capture

matrixParamSpec :: Spec
matrixParamSpec = do
  describe "Servant.API.MatrixParam" $ do
      it "allows to retrieve simple matrix parameters" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response1 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["a;name=bob"]
           }
          liftIO $ do
            decode' (simpleBody response1) `shouldBe` Just alice{
              name = "bob"
             }

      it "allows to retrieve lists in matrix parameters" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response2 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["b;names=bob;names=john", "bsub;names=anna;names=sarah"]
           }
          liftIO $
            decode' (simpleBody response2) `shouldBe` Just alice{
              name = "john"
             }

      it "allows to retrieve value-less matrix parameters" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response3 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["c;capitalize"]
           }
          liftIO $
            decode' (simpleBody response3) `shouldBe` Just alice{
              name = "ALICE"
             }

          response3' <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["c;capitalize="]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "ALICE"
             }

      it "allows to retrieve matrix parameters on captured segments" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response4 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["d", "12;name=stephen;capitalize", "dsub"]
           }
          liftIO $
            decode' (simpleBody response4) `shouldBe` Just alice{
              name = "STEPHEN",
              age = 12
             }

          response4' <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["d;ignored=1", "5", "dsub"]
           }
          liftIO $
            decode' (simpleBody response4') `shouldBe` Just alice{
              name = "Alice",
              age = 5
             }

type PostApi =
       ReqBody Person :> Post Integer
  :<|> "bla" :> ReqBody Person :> Post Integer
postApi :: Proxy PostApi
postApi = Proxy

postSpec :: Spec
postSpec = do
  describe "Servant.API.Post and .ReqBody" $ do
    with (return (serve postApi (return . age :<|> return . age))) $ do
      it "allows to POST a Person" $ do
        post "/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 201
         }

      it "allows alternative routes if all have request bodies" $ do
        post "/bla" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 201
         }

      it "handles trailing '/' gracefully" $ do
        post "/bla/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 201
         }

      it "correctly rejects invalid request bodies with status 400" $ do
        post "/" "some invalid body" `shouldRespondWith` 400


type RawApi = "foo" :> Raw
rawApi :: Proxy RawApi
rawApi = Proxy
rawApplication :: Show a => (Request -> a) -> Application
rawApplication f request_ respond = respond $ responseLBS ok200 [] (cs $ show $ f request_)

rawSpec :: Spec
rawSpec = do
  describe "Servant.API.Raw" $ do
    it "runs applications" $ do
      (flip runSession) (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo"]
         }
        liftIO $ do
          simpleBody response `shouldBe` "42"

    it "gets the pathInfo modified" $ do
      (flip runSession) (serve rawApi (rawApplication pathInfo)) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo", "bar"]
         }
        liftIO $ do
          simpleBody response `shouldBe` cs (show ["bar" :: String])


type AlternativeApi =
       "foo" :> Get Person
  :<|> "bar" :> Get Animal
unionApi :: Proxy AlternativeApi
unionApi = Proxy

unionServer :: Server AlternativeApi
unionServer =
       return alice
  :<|> return jerry

unionSpec :: Spec
unionSpec = do
  describe "Servant.API.Alternative" $ do
    with (return $ serve unionApi unionServer) $ do
      it "unions endpoints" $ do
        response <- get "/foo"
        liftIO $ do
          decode' (simpleBody response) `shouldBe`
            Just alice
        response_ <- get "/bar"
        liftIO $ do
          decode' (simpleBody response_) `shouldBe`
            Just jerry


type MonadApi = "foo" :> Get Integer
monadApi :: Proxy MonadApi
monadApi = Proxy

monadServer :: ServerT MonadApi (State Integer)
monadServer = lift $ do
  State.modify (+ 13)
  State.modify (* 2)
  State.get

monadSpec :: Spec
monadSpec = do
  describe "serveT with StateT monad" $ do
    with (return $ serveT monadApi (return . flip evalState 8) monadServer) $ do
      it "runs monadic computations" $ do
        get "/foo" `shouldRespondWith` "42"
      it "state is local to requests" $ do
        get "/foo" `shouldRespondWith` "42"
        get "/foo" `shouldRespondWith` "42"
        get "/foo" `shouldRespondWith` "42"

type EnterApi =
       "bar" :> Get Integer
  :<|> MonadApi
enterApi :: Proxy EnterApi
enterApi = Proxy

enterServer :: Server EnterApi
enterServer =
       return 42
  :<|> enter monadApi (return . flip evalState 6) monadServer

enterSpec :: Spec
enterSpec = do
  describe "combined API with different monads" $ do
    with (return $ serve enterApi enterServer) $ do
      it "runs requests in the IO monad" $ do
        get "/bar" `shouldRespondWith` "42"
      it "runs requests in the State monad" $ do
        get "/foo" `shouldRespondWith` "38"
