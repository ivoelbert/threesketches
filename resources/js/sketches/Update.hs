module Update where

{-# LANGUAGE OverloadedStrings #-}

import ThreeTypes

import Haste.Foreign
import Haste.JSON
import Haste.JSString
import Haste.Prim (toJSStr)

import Data.Set

-- Transformo a JSON

matrixToJson :: Matrix4 -> JSON
matrixToJson xss = Arr (Prelude.map (\x -> Num x) (Prelude.concat xss))

propertyToJson :: ObjTransform -> JSON
propertyToJson (SetPosition vect) =
  Dict [(pack "type", (Str (pack "setPosition"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (TranslateOnAxis vect dist) =
  Dict [(pack "type", (Str (pack "translateOnAxis"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect)), (pack("dist"), Num dist)]
propertyToJson (ApplyQuaternion quat) =
  Dict [(pack "type", (Str (pack "applyQuaternion"))), (pack "x", Num (qx quat)), (pack "y", Num (qy quat)), (pack "z", Num (qz quat)), (pack("w"), Num (qw quat))]
propertyToJson (RotateOnAxis vect radians) =
  Dict [(pack "type", (Str (pack "rotateOnAxis"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect)), (pack("radians"), Num radians)]
propertyToJson (SetUp vect) =
  Dict [(pack "type", (Str (pack "setUp"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (LookAt vect) =
  Dict [(pack "type", (Str (pack "lookAt"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (SetScale vect) =
  Dict [(pack "type", (Str (pack "setScale"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (ApplyMatrix xss) =
  Dict [(pack "type", (Str (pack "applyMatrix"))), (pack "matrix", matrixToJson xss)]

tupleToJson :: (String, [ObjTransform]) -> (JSString, JSON)
tupleToJson (s, obj) = let jsonArr = Prelude.map propertyToJson obj in (pack s, Arr jsonArr)

listToJson :: [(String, [ObjTransform])] -> JSON
listToJson xs = Dict (Prelude.map tupleToJson xs)

-- Batching...
batchTransformations :: [(String, ObjTransform)] -> [(String, [ObjTransform])]
batchTransformations xs = let names = getNames xs in Prelude.map (\n -> (n, getTransforms n xs)) names

getTransforms :: String -> [(String, ObjTransform)] -> [ObjTransform]
getTransforms name [] = []
getTransforms name ((n, transf):xs) = if name == n then transf:(getTransforms name xs) else (getTransforms name xs)

getNames :: [(String, ObjTransform)] -> [String]
getNames xs = mkUniq $ Prelude.map (Prelude.fst) xs

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . (Data.Set.fromList)


-- Interfaz:
createUpdate :: (Double -> ThreeAnimation ()) -> (Double -> JSString)
createUpdate f = (\frame -> let list = batchTransformations (Prelude.reverse (Prelude.snd (runThreeAnimation (f frame)))) in (encodeJSON (listToJson list)))
