module HaskellModules.ThreeTypes where

import Control.Applicative
import Control.Monad (liftM, ap)


data Quaternion = Quaternion {
                               qx :: Float,
                               qy :: Float,
                               qz :: Float,
                               qw :: Float
                             } deriving Show -- LTA gimbal lock

identityQ :: Quaternion
identityQ = Quaternion {qx = 0, qy = 0, qz = 0, qw = 1}

data Vector3 = Vector3 { vx :: Float,
                         vy :: Float,
                         vz :: Float
                       } deriving Show

vec0 :: Vector3
vec0 = Vector3 {vx = 0, vy = 0, vz = 0}

fromList :: [Float] -> Vector3
fromList (x:y:z:xs) = Vector3 {vx = x, vy = y, vz = z}

data Geometry = BoxGeometry { width :: Float,
                              height :: Float,
                              depth :: Float
                            }
              | ConeGeometry { radius :: Float,
                               height :: Float,
                               radialSegments :: Int
                             }
              | CylinderGeometry { radiusTop :: Float,
                                   radiusBottom :: Float,
                                   height :: Float,
                                   radialSegments :: Int
                                 }
              | SphereGeometry { radius :: Float,
                                 widthSegments :: Int,
                                 heightSegments :: Int
                               }
              | TorusGeometry { radius :: Float,
                                tube :: Float,
                                radialSegments :: Int,
                                tubularSegments :: Int
                              } deriving Show

data Color = RGB Float Float Float | HEX String deriving Show

data Material = StandardMaterial { materialColor :: Color,
                                   emissive :: Color,
                                   roughness :: Float,
                                   metalness :: Float
                                 } deriving Show

data Mesh = Mesh Geometry Material deriving Show

data Light = AmbientLight { lightColor :: Color,
                            intensity :: Float
                          }
           | DirectionalLight {
                                lightColor :: Color,
                                intensity :: Float,
                                target :: Vector3
                              }
           | PointLight {
                          lightColor :: Color,
                          intensity :: Float,
                          distance :: Float,
                          decay :: Float
                        } deriving Show

data Camera = PerspectiveCamera {
                                  fov :: Float,
                                  near :: Float,
                                  far :: Float
                                }
            | OrthographicCamera {
                                   near :: Float,
                                   far :: Float
                                 } deriving Show

data Object3D = ThreeMesh Mesh | ThreeLight Light | ThreeCamera Camera deriving Show


newtype ThreeScene a = Scene (a, [(String, Object3D)]) deriving Show

runThreeScene :: ThreeScene a -> (a, [(String, Object3D)])
runThreeScene (Scene p) = p


instance Functor ThreeScene where
  fmap = liftM

instance Applicative ThreeScene where
  pure  = return
  (<*>) = ap

instance Monad ThreeScene where
  return x = Scene (x, [])
  Scene (x, xs) >>= f = let Scene (x', xs') = f x
                        in Scene (x', xs' ++ xs)

newScene :: ThreeScene ()
newScene = Scene ((), [])

getSceneList :: ThreeScene a -> [(String, Object3D)]
getSceneList scn = snd (runThreeScene scn)

addToScene :: String -> Object3D -> ThreeScene ()
addToScene name obj = Scene ((), [(name, obj)])

createGeometry :: Geometry -> ThreeScene Geometry
createGeometry geom = return geom

createMaterial :: Material -> ThreeScene Material
createMaterial mat = return mat

createMesh :: Geometry -> Material -> ThreeScene Object3D
createMesh geom mat = return (ThreeMesh (Mesh geom mat))

createLight :: Light -> ThreeScene Object3D
createLight light = return (ThreeLight light)

createCamera :: Camera -> ThreeScene Object3D
createCamera camera = return (ThreeCamera camera)


{--

type Matrix4 = [[Float]]

identity4 :: Matrix4
identity4 = [[1, 0, 0, 0],
             [0, 1, 0, 0],
             [0, 0, 1, 0],
             [0, 0, 0, 1]]

data ObjProperties = ObjProperties {
                                     position :: Vector3,
                                     matrix :: Matrix4,
                                     quaternion :: Quaternion,
                                     eulerAngles :: Vector3,
                                     scale :: Vector3,
                                     up :: Vector3
                                   } deriving Show

baseProperties :: ObjProperties
baseProperties = ObjProperties {
                                 position = vec0,
                                 matrix = identity4,
                                 quaternion = identityQ,
                                 eulerAngles = vec0,
                                 scale = fromList [1, 1, 1],
                                 up = fromList [0, 1, 0]
                               }
--}
