-----------------------------------------------------------------------------
--
-- Module      :  ST.Matrix
-- Copyright   :  Michael Karg
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Binding to mjs library
--
-----------------------------------------------------------------------------

module Data.ST.Matrix where

import qualified Data.Matrix4 as M
import qualified Data.Matrix as M

import Control.Monad.Eff
import Control.Monad.ST (ST())

import Data.TypeNat
import Data.Array.Extended
import Data.Array.ST
import qualified Data.Vector as V
import Data.Array
import Data.Monoid
import Data.Function

import Data.Maybe.Unsafe (fromJust)
import Control.Apply
import Prelude.Unsafe
import Math


data STMat s h a = STMat [[a]] (STArray h a)
{-
instance sstm2 :: Sized (STMat Two h a) where
  sized v = 2
instance sstm3 :: Sized (STMat Three h a) where
  sized v = 3
instance sstm4 :: Sized (STMat Four h a) where
  sized v = 4

instance showSTMat2 :: (Show a) => Show (STMat Two h a) where
  show m = "Mat2x2 " ++ show (columns m)
instance showSTMat3 :: (Show a) => Show (STMat Three h a) where
  show m = "Mat3x3 " ++ show (columns m)
instance showSTMat4 :: (Show a) => Show (STMat Four h a) where
  show m = "Mat4x4 " ++ show (columns m)


columns :: forall s h a . (Sized (STMat s h a)) => STMat s h a -> [[a]]
columns mat@(STMat _ arr) =
    let m = unsafeFreeze arr
    in case sized mat of
        2 -> [slice 0 2 m, slice 2 4 m]
        3 -> [slice 0 3 m, slice 3 6 m, slice 6 9 m]
        4 -> [slice 0 4 m, slice 4 8 m, slice 8 12 m, slice 12 16 m]

-}

{-
instance stm2 :: M.Matrix (STMat Two h) a where
    generate = undef -- generate_ 2
instance stm3 :: M.Matrix (STMat Three h) a where
    generate = undef -- generate_ 3
instance stm4 :: M.Matrix (STMat Four h) a where
    generate = undef -- generate_ 4
-}


-- | Freeze an ST array. Do not mutate the STArray afterwards!
foreign import unsafeFreeze """
  function unsafeFreeze(arr) {
    return arr;
  }""" :: forall a h. STArray h a -> [a]

foreign import unsafeThaw """
  function unsafeThaw(arr) {
    return arr;
  }""" :: forall a h. [a] -> STArray h a



-- TODO unify slicing by splitting function in Data.Matrix 


-- careful, clears stack!
identityST :: forall s h r. (M.Matrix (M.Mat s) Number) => Eff (st :: ST h | r) (STMat s h Number)
identityST =
    let m = M.identity :: M.Mat s Number
    in STMat [] <$> thaw (M.toArray m)

transposeST :: forall s h r a. (M.Matrix (M.Mat s) a) => (STMat s h a) -> Eff (st :: ST h | r) (STMat s h a)
transposeST (STMat st arr) =
    let
        x   = unsafeFreeze arr
        m   = M.fromArray x :: M.Mat s a
        m'  = M.transpose m
        ar' = unsafeThaw $ M.toArray $ m'
    in return (STMat st ar')                                            -- TODO needs testing! 

{-
instance eqMat :: (Eq a) => Eq (Mat s a) where
  (==) (Mat l) (Mat r) = l == r
  (/=) (Mat l) (Mat r) = l /= r

instance functorMat :: Functor (Mat s) where
  (<$>) f (Mat l) = Mat (f <$> l)

instance applyMat :: Apply (Mat s) where
  (<*>) (Mat f) (Mat a) = Mat (zipWith (\f' a' -> f' a') f a)



-- | /O(1)/. Get an element of a matrix.
getElem :: forall s a. (Matrix (Mat s) a) =>
           Number      -- ^ Row
        -> Number      -- ^ Column
        -> Mat s a     -- ^ Matrix
        -> a
getElem i j m@(Mat l) = fromJust (l !! (i * sized m + j))
-}


foreign import scaleSTMatrixInt """
  function scaleSTMatrixInt(__dict_Num) {
      return function(x){
        return function(arr){
           return function(){
              var l=arr.length;
              for (var i=0; i<l; i++){
                arr[i] *= x;
              };
           };
        };
      };
  }""" :: forall a h r. (Num a) => a -> STArray h a -> Eff (st :: ST h | r) Unit

scaleSTMatrix :: forall s a h r. (Num a) => a -> (STMat s h a) -> Eff (st :: ST h | r) (STMat s h a)
scaleSTMatrix x v@(STMat _ arr) = scaleSTMatrixInt x arr *> return v

-- empties stack, careful!
fromMatrix :: forall s h r a. M.Mat s a -> Eff (st :: ST h | r) (STMat s h a)
fromMatrix (M.Mat m) = STMat [] <$> thaw m

stackPush :: forall s h r a. STMat s h a -> Eff (st :: ST h | r) (STMat s h a)
stackPush (STMat st arr) = do
    n <- freeze arr
    return (STMat (n:st) arr)
    
stackPop :: forall s h r a. STMat s h a -> Eff (st :: ST h | r) (STMat s h a)
stackPop v@(STMat st _) = case st of
    []      -> return v
    (x:xs)  -> let arr = unsafeThaw x in return (STMat xs arr)


foreign import runSTMatrixInt """
  function runSTMatrixInt(f) {
    return function(){
        var res = f();
        return (res.value1);
    };
  }""" :: forall s a r. (forall h. Eff (st :: ST h | r) (STMat s h a)) -> Eff r [a]

runSTMatrix :: forall s a r. (forall h. Eff (st :: ST h | r) (STMat s h a)) -> Eff r (M.Mat s a)
runSTMatrix eff = M.Mat <$> runSTMatrixInt eff



