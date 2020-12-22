{-# LANGUAGE QuasiQuotes #-}
module HTTP.APISpec where

import           ClassyPrelude
import           HTTP.API
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

httpSpecs :: Spec
httpSpecs = with (return myApp) $ 
    describe "Platform.HTTP" $ do
        spec_api
        spec_apiLangs

spec_api = describe "GET /api/hello" $
            it "responds with 200 / 'hello'" $
                get "/api/hello" `shouldRespondWith` "hello" {matchStatus = 200}

spec_apiLangs = describe "GET /api/langs" $  do
            it "responds with 200" $
                get "/api/langs" `shouldRespondWith` 200
            it "responds with [Titan,ProtoHuman,English]" $ 
                get "/api/langs" `shouldRespondWith` 
                    [json|
                    [{"name":"Titan"},{"name":"ProtoHuman"},{"name":"English"}]
                    |]
