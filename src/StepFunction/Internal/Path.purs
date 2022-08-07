module StepFunction.Internal.Path where

import Prim.Row (class Cons, class Nub)
import Prim.Symbol as Sym

data Path :: forall k. Symbol -> k -> Type
data Path h t = Path

data Nil

class PathParser :: forall k. Symbol -> Symbol -> Symbol -> k -> Constraint
class PathParser acc h t o | h t -> o acc

instance PathParser acc "^" "" (Path acc Nil)
else instance
  ( Sym.Cons h t i
  , PathParser "" h t o
  ) =>
  PathParser acc "." i (Path acc o)
else instance
  ( Sym.Cons h' t' t
  , Sym.Append acc h acc'
  , PathParser acc' h' t' o
  ) =>
  PathParser acc h t o

class PathMaker :: forall k. Symbol -> k -> Constraint
class PathMaker k p | k -> p

instance
  ( Sym.Append k "^" k'
  , Sym.Cons h t k'
  , PathParser "" h t (Path ph pt)
  ) =>
  PathMaker k (Path ph pt)

class PathGetter :: forall k. k -> Type -> Type -> Constraint
class PathGetter p i o | p i -> o

instance PathGetter Nil i i
else instance
  ( PathGetter t i o
  ) =>
  PathGetter (Path "$" t) i o
else instance
  ( Cons name ty r' r
  , PathGetter t ty o
  ) =>
  PathGetter (Path name t) { | r } o

class PathSetter :: forall k. k -> Type -> Type -> Type -> Constraint
class PathSetter p t i o | p t i -> o

instance PathSetter Nil t i t
else instance (PathSetter p t i o) => PathSetter (Path "$" p) t i o
else instance
  ( Cons name ty r r'
  , Nub r' o
  ) =>
  PathSetter (Path name Nil) ty { | r } { | o }
else instance
  ( Cons name f r' r
  , PathSetter t ty f f'
  , Cons name f' r' o
  ) =>
  PathSetter (Path name t) ty { | r } { | o }
