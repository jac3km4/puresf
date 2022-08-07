module Test.Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (stringify) as Json
import Data.Argonaut.Encode (encodeJson) as Json
import Effect (Effect)
import Effect.Aff (launchAff_)
import StepFunction (Path(..), Lambda(..), StepOp)
import StepFunction as SF
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

newtype TestJson = TestJson Json

derive newtype instance Eq TestJson

instance Show TestJson where
  show (TestJson json) = Json.stringify json

type User = { name :: String }

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Step Function codegen" do

    it "generates correct JSON representation with Map" do
      let
        stepOp :: StepOp (Array User) (Array { name :: String, age :: Int })
        stepOp =
          SF.map (Path :: Path "$") (Path :: Path "$")
            $ SF.invoke stringIntLambda (Path :: Path "$.name") (Path :: Path "$.age")

      shouldEqual (TestJson (SF.encode stepOp))
        $ TestJson
        $ Json.encodeJson
            { "States":
                { "Step1":
                    { "Type": "Map"
                    , "ItemsPath": "$"
                    , "ResultPath": "$"
                    , "Iterator":
                        { "States":
                            { "Step2":
                                { "Type": "Task"
                                , "Resource": "arn2"
                                , "InputPath": "$.name"
                                , "ResultPath": "$.age"
                                }
                            }
                        , "StartAt": "Step2"
                        }
                    }
                }
            , "StartAt": "Step1"
            }

    it "generates correct JSON representation with Compose" do
      let
        stepOp :: StepOp User Int
        stepOp =
          SF.pass (Path :: Path "$.copy")
            >>> SF.invoke userStringLambda (Path :: Path "$.copy") (Path :: Path "$")
            >>> SF.invoke stringIntLambda (Path :: Path "$") (Path :: Path "$")

      shouldEqual (TestJson (SF.encode stepOp))
        $ TestJson
        $ Json.encodeJson
            { "States":
                { "Step1":
                    { "Type": "Pass"
                    , "ResultPath": "$.copy"
                    , "Next": "Step2"
                    }
                , "Step2":
                    { "Type": "Task"
                    , "Resource": "arn1"
                    , "InputPath": "$.copy"
                    , "ResultPath": "$"
                    , "Next": "Step3"
                    }
                , "Step3":
                    { "Type": "Task"
                    , "Resource": "arn2"
                    , "InputPath": "$"
                    , "ResultPath": "$"
                    }
                }
            , "StartAt": "Step1"
            }
  where

  userStringLambda :: Lambda User String
  userStringLambda = Lambda "arn1"

  stringIntLambda :: Lambda String Int
  stringIntLambda = Lambda "arn2"
