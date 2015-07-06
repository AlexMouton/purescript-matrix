-----------------------------------------------------------------------------
--
-- Module      :  ST.Matrix4
-- Copyright   :  Michael Karg
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Inspired by Mjs library for javascript
--
-----------------------------------------------------------------------------

module Data.ST.Matrix4 where

import Prelude
import Data.TypeNat
import Data.Matrix4 (Vec3N())
import Data.ST.Matrix
import qualified Data.Vector3 as V3
import qualified Data.Vector as V
import Control.Monad.Eff
import Control.Monad.ST (ST())
-- import Control.Apply
import Data.Array
import Data.Array.ST hiding (freeze, thaw)
import Math


type STMat4 h = STMat Four h Number

foreign import identityST  :: forall h r. Eff (st :: ST h | r) (STMat Four h Number)

foreign import rotateST  :: forall h r. Number -> Vec3N -> STMat4 h -> Eff (st :: ST h | r) Unit

-- generic type was :: forall h r. Number -> [Number] -> STArray h Number -> Eff (st :: ST h | r) Unit

foreign import translateST  :: forall h r. Vec3N -> STMat4 h -> Eff (st :: ST h | r) Unit

-- generic type was :: forall h r. [Number] -> STArray h Number -> Eff (st :: ST h | r) Unit

foreign import scaleST3  :: forall h r. Number -> Number -> Number -> STMat4 h -> Eff (st :: ST h | r) Unit
