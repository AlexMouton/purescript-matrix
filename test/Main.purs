
module Test.Main where

import Prelude
import Data.Matrix4 (Mat4, Vec3N, identity, translate, rotate) as M
import Data.ST.Matrix4 (STMat4, translateST, rotateST, identityST) as M
import Data.ST.Matrix (runSTMatrix) as M

import Test.Spec (pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Data.Vector as V

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST())
import Control.Monad.Eff.Console (logShow, CONSOLE)


ble :: forall h r . Eff (st :: ST h | r) (M.STMat4 h)
-- ble = fromMatrix (M.fromArrayColumns [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]) >>= stackPush >>= scaleSTMatrix 2 >>= stackPop
-- ble = M.identityST >>= M.scaleSTMatrix 2
ble = M.identityST

meh :: M.Vec3N
meh = V.Vec [0.5,1.5,0.9]

ys :: M.Mat4
ys = M.rotate 90.0 meh $ M.identity

zs :: M.Mat4
zs = M.translate meh $ M.identity

preexisting :: forall e. Eff ( console :: CONSOLE | e ) Unit
preexisting = do
  xs <- M.runSTMatrix (ble >>= \m -> M.rotateST 90.0 meh m *> pure m)
  bs <- M.runSTMatrix (ble >>= \m -> M.translateST meh m *> pure m)

  logShow xs
  logShow ys

  logShow bs
  logShow zs

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  pending "tests"
