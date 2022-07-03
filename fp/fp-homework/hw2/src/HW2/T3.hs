module HW2.T3
        ( joinOption,
        joinExcept,
        joinAnnotated,
        joinList,
        joinFun
        ) where

import           HW2.T1 (Annotated (..), Except (..), Fun (..), List (..),
                         Option (..))
import           HW2.T2 (concatLists)

joinOption    :: Option (Option a) -> Option a
joinOption (Some x) = x
joinOption _        = None

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Success x) = x
joinExcept (Error x)   = Error x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((expr :# an1) :# an2) = expr:#(an2 <> an1)

joinList      :: List (List a) -> List a
joinList Nil    = Nil
joinList (h:.Nil) = h
joinList (h:.t) = concatLists h (joinList t)

unwrapFun :: Fun i a -> (i -> a)
unwrapFun (F f) = f

joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> (unwrapFun (f i)) i)
