{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Units.Dimensional.DK.Vector () where
import Numeric.Units.Dimensional.DK (Quantity)

import Control.Monad (liftM)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Base as U

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))

import Unsafe.Coerce (unsafeCoerce)

instance Storable a => Storable (Quantity d a) where
  sizeOf _ = sizeOf (undefined::a)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::a)
  {-# INLINE alignment #-}
  poke ptr = poke (castPtr ptr :: Ptr a) . unsafeCoerce
  {-# INLINE poke #-}
  peek ptr = liftM unsafeCoerce (peek (castPtr ptr :: Ptr a))
  {-# INLINE peek #-}

newtype instance U.Vector (Quantity d a)    =  V_Quantity {unVQ :: U.Vector a}
newtype instance U.MVector s (Quantity d a) = MV_Quantity {unMVQ :: U.MVector s a}
instance U.Unbox a => U.Unbox (Quantity d a)

instance (M.MVector U.MVector a) => M.MVector U.MVector (Quantity d a) where
  basicLength = M.basicLength . unMVQ
  basicUnsafeSlice m n = MV_Quantity . M.basicUnsafeSlice m n . unMVQ
  basicOverlaps u v = M.basicOverlaps (unMVQ u) (unMVQ v)
  basicUnsafeNew = liftM MV_Quantity . M.basicUnsafeNew
  basicUnsafeRead v = liftM unsafeCoerce . M.basicUnsafeRead (unMVQ v)
  basicUnsafeWrite v i = M.basicUnsafeWrite (unMVQ v) i . unsafeCoerce
#if MIN_VERSION_vector(0,11,0)
  basicInitialize = M.basicInitialize . unMVQ
#endif

instance (G.Vector U.Vector a) => G.Vector U.Vector (Quantity d a) where
  basicUnsafeFreeze = liftM V_Quantity  . G.basicUnsafeFreeze . unMVQ
  basicUnsafeThaw   = liftM MV_Quantity . G.basicUnsafeThaw   . unVQ
  basicLength       = G.basicLength . unVQ
  basicUnsafeSlice m n = V_Quantity . G.basicUnsafeSlice m n . unVQ
  basicUnsafeIndexM v = liftM unsafeCoerce . G.basicUnsafeIndexM (unVQ v)
