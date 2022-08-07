module StepFunction.Internal.Dsl (map, invoke, pass) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import StepFunction.Internal.Path (class PathGetter, class PathMaker, class PathSetter)
import StepFunction.Internal.Path as Internal
import StepFunction.Internal.Types (StepOp(..), Lambda(..), Path)
import Type.Proxy (Proxy(..))

pass
  :: forall opath oph opt i o
   . IsSymbol opath
  => PathMaker opath (Internal.Path oph opt)
  => PathSetter (Internal.Path oph opt) i i o
  => Path opath
  -> StepOp i o
pass _ = Pass { outputPath }
  where
  outputPath = reflectSymbol (Proxy :: Proxy opath)

invoke
  :: forall ipath opath iph ipt oph opt i o ri ro
   . IsSymbol ipath
  => IsSymbol opath
  => PathMaker ipath (Internal.Path iph ipt)
  => PathGetter (Internal.Path iph ipt) ri i
  => PathMaker opath (Internal.Path oph opt)
  => PathSetter (Internal.Path oph opt) o ri ro
  => Lambda i o
  -> Path ipath
  -> Path opath
  -> StepOp ri ro
invoke (Lambda arn) _ _ =
  Invoke { arn, inputPath, outputPath }
  where
  inputPath = reflectSymbol (Proxy :: Proxy ipath)
  outputPath = reflectSymbol (Proxy :: Proxy opath)

map
  :: forall ipath opath iph ipt oph opt i o ri ro
   . IsSymbol ipath
  => IsSymbol opath
  => PathMaker ipath (Internal.Path iph ipt)
  => PathGetter (Internal.Path iph ipt) ri (Array i)
  => PathMaker opath (Internal.Path oph opt)
  => PathSetter (Internal.Path oph opt) (Array o) ri ro
  => Path ipath
  -> Path opath
  -> StepOp i o
  -> StepOp ri ro
map _ _ it =
  Map { iter: (_ $ it), inputPath, outputPath }
  where
  inputPath = reflectSymbol (Proxy :: Proxy ipath)
  outputPath = reflectSymbol (Proxy :: Proxy opath)
