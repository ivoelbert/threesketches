module ThreeTypes where

import Control.Applicative
import Control.Monad (liftM, ap)


type Matrix4 = [[Double]]

identity4 :: Matrix4
identity4 = [[1, 0, 0, 0],
             [0, 1, 0, 0],
             [0, 0, 1, 0],
             [0, 0, 0, 1]]

data Quaternion = Quaternion {
                               qx :: Double,
                               qy :: Double,
                               qz :: Double,
                               qw :: Double
                             } deriving Show -- LTA gimbal lock

identityQ :: Quaternion
identityQ = Quaternion {qx = 0, qy = 0, qz = 0, qw = 1}

data Vector3 = Vector3 { vx :: Double,
                         vy :: Double,
                         vz :: Double
                       } deriving Show

vec0 :: Vector3
vec0 = Vector3 {vx = 0, vy = 0, vz = 0}

listToVector :: [Double] -> Vector3
listToVector (x:y:z:xs) = Vector3 {vx = x, vy = y, vz = z}

vector :: Double -> Double -> Double -> Vector3
vector x y z = Vector3 {vx = x, vy = y, vz = z}

data Geometry = BoxGeometry { width :: Double,
                              height :: Double,
                              depth :: Double
                            }
              | ConeGeometry { radius :: Double,
                               height :: Double,
                               radialSegments :: Int
                             }
              | CylinderGeometry { radiusTop :: Double,
                                   radiusBottom :: Double,
                                   height :: Double,
                                   radialSegments :: Int
                                 }
              | SphereGeometry { radius :: Double,
                                 widthSegments :: Int,
                                 heightSegments :: Int
                               }
              | TorusGeometry { radius :: Double,
                                tube :: Double,
                                radialSegments :: Int,
                                tubularSegments :: Int
                              } deriving Show

data Color = RGB Double Double Double | HEX String deriving Show

data Material = StandardMaterial { materialColor :: Color,
                                   emissive :: Color,
                                   roughness :: Double,
                                   metalness :: Double
                                 } deriving Show

data Mesh = Mesh Geometry Material deriving Show

data Light = AmbientLight { lightColor :: Color,
                            intensity :: Double
                          }
           | DirectionalLight {
                                lightColor :: Color,
                                intensity :: Double,
                                target :: Vector3
                              }
           | PointLight {
                          lightColor :: Color,
                          intensity :: Double,
                          distance :: Double,
                          decay :: Double
                        } deriving Show

data Camera = PerspectiveCamera {
                                  fov :: Double,
                                  near :: Double,
                                  far :: Double
                                }
            | OrthographicCamera {
                                   near :: Double,
                                   far :: Double
                                 } deriving Show

data Object3D = ThreeMesh Mesh | ThreeLight Light | ThreeCamera Camera deriving Show


data ObjTransform = SetPosition Vector3 --Setea la posicion del objeto
                  | TranslateOnAxis Vector3 Double --Traslada el objeto cierta distancia por un vector
                  | ApplyQuaternion Quaternion --Aplica la rotacion representada por el cuaternion
                  | RotateOnAxis Vector3 Double --Rota el objeto ciertos radianes con respecto a un vector
                  | SetUp Vector3 --Setea el vector "up", util para LookAt
                  | LookAt Vector3 --Rota un objeto para que "mire" hacia un punto.
                  | SetScale Vector3 --Setea la escala del objeto en los ejes x y z
                  | ApplyMatrix Matrix4 deriving Show --Premultiplica la matriz de transformacion del objeto por la indicada


{-- Sirve para representar un frame de animacion en el que a un objeto se le actualizan ciertas propiedades --}
newtype ThreeAnimation a = Anim (a, [(String, ObjTransform)]) deriving Show

runThreeAnimation :: ThreeAnimation a -> (a, [(String, ObjTransform)])
runThreeAnimation (Anim p) = p

instance Functor ThreeAnimation where
  fmap = liftM

instance Applicative ThreeAnimation where
  pure  = return
  (<*>) = ap

instance Monad ThreeAnimation where
  return x = Anim (x, [])
  Anim (x, xs) >>= f = let Anim (x', xs') = f x
                        in Anim (x', xs' ++ xs)

newAnimation :: ThreeAnimation ()
newAnimation = Anim ((), [])

translate :: String -> Vector3 -> Double -> ThreeAnimation ()
translate name dir dist = Anim ( (), [(name, TranslateOnAxis dir dist)] )

setPosition :: String -> Vector3 -> ThreeAnimation ()
setPosition name pos = Anim ( (), [(name, SetPosition pos)])

{-- Representa una escena. Basicamente una lista de objetos con un nombre para poder actualizar en la animacion --}
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
