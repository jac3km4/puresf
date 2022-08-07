module StepFunction.Internal.Encoding (encode) where

import Prelude

import Control.Monad.State (State)
import Control.Monad.State as S
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import StepFunction.Internal.Types (Compose(..), StepOp(..))

encode :: forall i o. StepOp i o -> Json
encode = flip S.evalState 0 <<< encodeDef

encodeDef :: forall i o. StepOp i o -> State Int Json
encodeDef op = do
  start <- S.get
  steps <- encodeSteps op
  let
    obj = Object.fromFoldable $ mapFlipped steps $ stepName *** Json.fromObject
  pure $ Json.fromObject $ Object.fromHomogeneous
    { "StartAt": Json.fromString $ stepName $ start + 1
    , "States": Json.fromObject obj
    }

encodeSteps :: forall i o. StepOp i o -> State Int (Array (Tuple Int (Object Json)))
encodeSteps = case _ of
  Pass { outputPath } -> do
    i <- increment
    pure $ wrap i $ Object.fromHomogeneous $
      { "Type": Json.fromString "Pass"
      , "ResultPath": Json.fromString outputPath
      }
  Invoke { arn, inputPath, outputPath } -> do
    i <- increment
    pure $ wrap i $ Object.fromHomogeneous $
      { "Type": Json.fromString "Task"
      , "Resource": Json.fromString arn
      , "InputPath": Json.fromString inputPath
      , "ResultPath": Json.fromString outputPath
      }
  Map { iter, inputPath, outputPath } -> do
    i <- increment
    it <- iter encodeDef
    pure $ wrap i $ Object.fromHomogeneous $
      { "Type": Json.fromString "Map"
      , "Iterator": it
      , "ItemsPath": Json.fromString inputPath
      , "ResultPath": Json.fromString outputPath
      }
  Compose fa -> do
    let Tuple fst snd = fa \(ComposeStep snd fst) -> Tuple (encodeSteps fst) (encodeSteps snd)
    fst' <- fst
    next <- S.get
    snd' <- snd
    let
      fst'' = case Array.unsnoc fst' of
        Just { init, last: Tuple idx el } ->
          Array.snoc init $ Tuple idx $ Object.insert "Next" (Json.fromString $ stepName (next + 1)) el
        Nothing -> fst'
    pure $ Array.concat [ fst'', snd' ]

  where
  increment = S.modify (_ + 1)
  wrap i = Array.singleton <<< Tuple i

stepName :: Int -> String
stepName i = "Step" <> show i
