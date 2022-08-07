module StepFunction.Internal.Types
  ( Compose(..)
  , StepOp(..)
  , Lambda(..)
  , Path(..)
  , Exists
  , Exists2
  ) where

import Prelude

data StepOp :: Type -> Type -> Type
data StepOp i o
  = Invoke { arn :: String, inputPath :: String, outputPath :: String }
  | Compose (Exists (Compose i o))
  | Map { iter :: (Exists2 StepOp), inputPath :: String, outputPath :: String }
  | Pass { outputPath :: String }

instance Semigroupoid StepOp where
  compose a b = Compose (_ $ ComposeStep a b)

data Compose i o im = ComposeStep (StepOp im o) (StepOp i im)

newtype Map = MapStep (Exists2 StepOp)

newtype Lambda :: Type -> Type -> Type
newtype Lambda i o = Lambda String

data Path :: forall k. k -> Type
data Path a = Path

type Exists :: forall k. (k -> Type) -> Type
type Exists f = ∀ r. (∀ a. f a -> r) -> r

type Exists2 :: forall k1 k2. (k1 -> k2 -> Type) -> Type
type Exists2 f = ∀ r. (∀ a b. f a b -> r) -> r
