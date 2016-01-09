module Foreign.Storable.Unit where

import Foreign.Storable (Storable (..), )


instance Storable () where
   sizeOf    _ = 0
   alignment _ = 1
   peek      _ = return ()
   poke    _ _ = return ()
