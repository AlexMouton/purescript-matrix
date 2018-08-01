
module Test.Main where

import Prelude

import Test.Spec (describe, it)
import Test.Spec.Assertions as A
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Data.TypeNat (Four, Three)

import Data.Array (length)
import Data.Int (toNumber)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Vector as V
import Data.Matrix (Mat, columns, generate, toArrayColumns) as M
import Data.Matrix4 (Mat4, Vec3N, identity, rotate, translate) as M
import Data.ST.Matrix (runSTMatrix) as M
import Data.ST.Matrix4 (STMat4, translateST, rotateST, identityST) as M


import Effect (Effect)
import Effect.Console (logShow)

ble :: forall h. Effect (M.STMat4 h)
-- ble = fromMatrix (M.fromArrayColumns [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]) >>= stackPush >>= scaleSTMatrix 2 >>= stackPop
-- ble = M.identityST >>= M.scaleSTMatrix 2
ble = M.identityST

meh :: M.Vec3N
meh = V.Vec [0.5,1.5,0.9]

ys :: M.Mat4
ys = M.rotate 90.0 meh $ M.identity

zs :: M.Mat4
zs = M.translate meh $ M.identity

preexisting :: Effect Unit
preexisting = do
  xs <- M.runSTMatrix (ble >>= \m -> M.rotateST 90.0 meh m *> pure m)
  bs <- M.runSTMatrix (ble >>= \m -> M.translateST meh m *> pure m)

  logShow xs
  logShow ys

  logShow bs
  logShow zs

main :: Effect Unit
main = run [consoleReporter] do
  describe "columns" do
    describe "for a given matrix" do
      let m = M.generate (\i j -> toNumber j) :: M.Mat Three Four Number
      let result = M.columns m
      it "gives a row worth of columns" do
        A.shouldEqual (length result) 4

  describe "generate" do
    describe "for a given matrix" do
      let result = M.generate (\i j -> i /\ j) :: M.Mat Three Four (Tuple Int Int)
      it "is zero indexed" do
        A.shouldContain (M.toArrayColumns result) (0 /\ 0)
      it "does not invert rows and columns" do
        A.shouldContain (M.toArrayColumns result) (2 /\ 3)
        A.shouldNotContain (M.toArrayColumns result) (3 /\ 2)
