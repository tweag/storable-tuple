{-
This should be in the standard library.
-}
module Foreign.Storable.Tuple where

import Data.Orphans ()
import Foreign.Storable (Storable (..), )
import qualified Foreign.Storable.Record as Store
import Control.Applicative (liftA2, liftA3, pure, (<*>), )

import Data.Tuple.HT (fst3, snd3, thd3, )


instance (Storable a, Storable b) => Storable (a,b) where
   sizeOf    = Store.sizeOf storePair
   alignment = Store.alignment storePair
   peek      = Store.peek storePair
   poke      = Store.poke storePair

{-# INLINE storePair #-}
storePair ::
   (Storable a, Storable b) =>
   Store.Dictionary (a,b)
storePair =
   Store.run $
   liftA2 (,)
      (Store.element fst)
      (Store.element snd)


instance (Storable a, Storable b, Storable c) => Storable (a,b,c) where
   sizeOf    = Store.sizeOf storeTriple
   alignment = Store.alignment storeTriple
   peek      = Store.peek storeTriple
   poke      = Store.poke storeTriple

{-# INLINE storeTriple #-}
storeTriple ::
   (Storable a, Storable b, Storable c) =>
   Store.Dictionary (a,b,c)
storeTriple =
   Store.run $
   liftA3 (,,)
      (Store.element fst3)
      (Store.element snd3)
      (Store.element thd3)

instance (Storable a, Storable b, Storable c, Storable d) => Storable (a,b,c,d) where
   sizeOf    = Store.sizeOf storeQuadruple
   alignment = Store.alignment storeQuadruple
   peek      = Store.peek storeQuadruple
   poke      = Store.poke storeQuadruple

{-# INLINE storeQuadruple #-}
storeQuadruple ::
   (Storable a, Storable b, Storable c, Storable d) =>
   Store.Dictionary (a,b,c,d)
storeQuadruple =
   Store.run $
   pure (\a b c d -> (a,b,c,d))
      <*> (Store.element $ \(x,_,_,_) -> x)
      <*> (Store.element $ \(_,x,_,_) -> x)
      <*> (Store.element $ \(_,_,x,_) -> x)
      <*> (Store.element $ \(_,_,_,x) -> x)
{-
   liftA4 (,,,)
      (Store.element $ \(x,_,_,_) -> x)
      (Store.element $ \(_,x,_,_) -> x)
      (Store.element $ \(_,_,x,_) -> x)
      (Store.element $ \(_,_,_,x) -> x)
-}


{-
{- Why is this allowed? -}
test :: Char
test = const 'a' undefined

{- Why is type defaulting applied here? The type of 'c' should be fixed. -}
test1 :: (Integral a, RealField.C a) => a
test1 =
   let c = undefined
   in  asTypeOf (round c) c
-}
