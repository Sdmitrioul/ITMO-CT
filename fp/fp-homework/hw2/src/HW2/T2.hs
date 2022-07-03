module HW2.T2
        ( distOption,
        distPair,
        distQuad,
        distAnnotated,
        distExcept,
        distPrioritised,
        distStream,
        distList,
        distFun,
        wrapOption,
        wrapPair,
        wrapQuad,
        wrapAnnotated,
        wrapExcept,
        wrapPrioritised,
        wrapStream,
        wrapList,
        wrapFun,
        concatLists
        ) where

import HW2.T1 (
          Option(..),
          Pair(..), Quad(..),
          Annotated(..),
          Except(..),
          Prioritised(..),
          Stream(..),
          List(..),
          Fun(..),
          mapList
          )

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)         = None
distOption (_, None)            = None
distOption ((Some x), (Some y)) = Some (x, y)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair ((P x y), (P a b)) = P (x, a) (y, b)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad ((Q a b c d), (Q x y w z)) = Q (a, x) (b, y) (c, w) (d, z)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated ((an1 :# expr1), (an2 :# expr2)) = (an1, an2):#(expr1 <> expr2)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error er, _)          = Error er
distExcept (_, Error er)          = Error er
distExcept (Success a, Success b) = Success (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised ((High a), (High b))     = High (a, b)
distPrioritised ((High a), (Medium b))   = High (a, b)
distPrioritised ((Medium a), (High b))   = High (a, b)
distPrioritised ((High a), (Low b))      = High (a, b)
distPrioritised ((Low a), (High b))      = High (a, b)
distPrioritised ((Medium a), (Medium b)) = Medium (a, b)
distPrioritised ((Medium a), (Low b))    = Medium (a, b)
distPrioritised ((Low a), (Medium b))    = Medium (a, b)
distPrioritised ((Low a), (Low b))       = Low (a, b)

distStream      :: (Stream a, Stream b) -> Stream (a, b)
distStream ((x:>s), (y:>ss)) = (x, y):>(distStream (s, ss))

distList        :: (List a, List b) -> List (a, b)
distList (Nil, _)       = Nil
distList (_, Nil)       = Nil
distList ((xh:.xt), yl) = concatLists (mapList (\x -> (xh, x)) yl) (distList (xt, yl))

distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun ((F f), (F g)) = F (\x -> ((f x), (g x)))

wrapOption      :: a -> Option a
wrapOption x = Some x

wrapPair        :: a -> Pair a
wrapPair x = P x x

wrapQuad        :: a -> Quad a
wrapQuad x = Q x x x x

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated x = x:#mempty

wrapExcept      :: a -> Except e a
wrapExcept x = Success x

wrapPrioritised :: a -> Prioritised a
wrapPrioritised x = Low x

wrapStream      :: a -> Stream a
wrapStream x = x:>(wrapStream x)

wrapList        :: a -> List a
wrapList x = x:.Nil

wrapFun         :: a -> Fun i a
wrapFun x = F (\_ -> x)

---Helper-------------------------------------------------

concatLists :: List a -> List a -> List a
concatLists Nil x    = x
concatLists (h:.t) x = h:.(concatLists t x)