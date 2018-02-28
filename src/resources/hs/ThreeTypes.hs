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

quaternion :: Double -> Double -> Double -> Double -> Quaternion
quaternion x y z w = Quaternion {qx = x, qy = y, qz = z, qw = w}

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


data Color = RGB Double Double Double | HEX String deriving Show

rgb :: Double -> Double -> Double -> Color
rgb r g b = RGB r g b

hex :: String -> Color
hex h = HEX h


{---------------------------------------------------------------------------------------------------}
{-- Representa un frame de animacion en el que a un objeto se le aplican ciertas transformaciones --}
{---------------------------------------------------------------------------------------------------}
data ObjTransform = SetPosition Vector3 --Setea la posicion del objeto
                  | TranslateOnAxis Vector3 Double --Traslada el objeto cierta distancia por un vector
                  | ApplyQuaternion Quaternion --Aplica la rotacion representada por el cuaternion
                  | RotateOnAxis Vector3 Double --Rota el objeto ciertos radianes con respecto a un vector
                  | SetUp Vector3 --Setea el vector "up", util para LookAt
                  | LookAt Vector3 --Rota un objeto para que "mire" hacia un punto.
                  | SetScale Vector3 --Setea la escala del objeto en los ejes x y z
                  | ApplyMatrix Matrix4 deriving Show --Premultiplica la matriz de transformacion del objeto por la indicada

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


-- Transformations
addTransform :: String -> ObjTransform -> ThreeAnimation ()
addTransform name transf = Anim ( (), [(name, transf)])

setPosition :: String -> Vector3 -> ThreeAnimation ()
setPosition name pos = addTransform name (SetPosition pos)

translateOnAxis :: String -> Vector3 -> Double -> ThreeAnimation ()
translateOnAxis name dir dist = addTransform name (TranslateOnAxis dir dist)

applyQuaternion :: String -> Quaternion -> ThreeAnimation ()
applyQuaternion name quat = addTransform name (ApplyQuaternion quat)

rotateOnAxis :: String -> Vector3 -> Double -> ThreeAnimation ()
rotateOnAxis name axis angle = addTransform name (RotateOnAxis axis angle)

setUp :: String -> Vector3 -> ThreeAnimation ()
setUp name up = addTransform name (SetUp up)

lookAt :: String -> Vector3 -> ThreeAnimation ()
lookAt name pos = addTransform name (LookAt pos)

setScale :: String -> Vector3 -> ThreeAnimation ()
setScale name scl = addTransform name (SetScale scl)

applyMatrix :: String -> Matrix4 -> ThreeAnimation ()
applyMatrix name mat = addTransform name (ApplyMatrix mat)

{-----------------------------------------------------------------------------------------------------------------}
{-- Representa una escena. Basicamente una lista de objetos con un nombre para poder actualizar en la animacion --}
{-----------------------------------------------------------------------------------------------------------------}
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


-- Geometries
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

createGeometry :: Geometry -> ThreeScene Geometry
createGeometry geom = return geom

boxGeometry :: Double -> Double -> Double -> ThreeScene Geometry
boxGeometry w h d = createGeometry (BoxGeometry {width = w, height = h, depth = d})

coneGeometry :: Double -> Double -> Int -> ThreeScene Geometry
coneGeometry r h rs = createGeometry (ConeGeometry {radius = r, height = h, radialSegments = rs})

cylinderGeometry :: Double -> Double -> Double -> Int -> ThreeScene Geometry
cylinderGeometry rt rb h rs = createGeometry (CylinderGeometry {radiusTop = rt, radiusBottom = rb, height = h, radialSegments = rs})

sphereGeometry :: Double -> Int -> Int -> ThreeScene Geometry
sphereGeometry r ws hs = createGeometry (SphereGeometry {radius = r, widthSegments = ws, heightSegments = hs})

torusGeometry :: Double -> Double -> Int -> Int -> ThreeScene Geometry
torusGeometry r t rs ts = createGeometry (TorusGeometry {radius = r, tube = t, radialSegments = rs, tubularSegments = ts})


-- Materials
data Material = StandardMaterial { materialColor :: Color,
                                   emissive :: Color,
                                   roughness :: Double,
                                   metalness :: Double
                                 } deriving Show

createMaterial :: Material -> ThreeScene Material
createMaterial mat = return mat

standardMaterial :: Color -> Color -> Double -> Double -> ThreeScene Material
standardMaterial c e r m = createMaterial (StandardMaterial {materialColor = c, emissive = e, roughness = r, metalness = m})


-- Mesh
data Mesh = Mesh Geometry Material deriving Show

createMesh :: Geometry -> Material -> ThreeScene Object3D
createMesh geom mat = return (ThreeMesh (Mesh geom mat))


-- Lights
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

createLight :: Light -> ThreeScene Object3D
createLight light = return (ThreeLight light)

ambientLight :: Color -> Double -> ThreeScene Object3D
ambientLight c i = createLight (AmbientLight {lightColor = c, intensity = i})

directionalLight :: Color -> Double -> Vector3 -> ThreeScene Object3D
directionalLight c i t = createLight (DirectionalLight {lightColor = c, intensity = i, target = t})

pointLight :: Color -> Double -> Double -> Double -> ThreeScene Object3D
pointLight c i di de = createLight (PointLight {lightColor = c, intensity = i, distance = di, decay = de})


-- Cameras
data Camera = PerspectiveCamera {
                                  fov :: Double,
                                  near :: Double,
                                  far :: Double
                                }
            | OrthographicCamera {
                                   near :: Double,
                                   far :: Double
                                 } deriving Show

createCamera :: Camera -> ThreeScene Object3D
createCamera camera = return (ThreeCamera camera)

perspectiveCamera :: Double -> Double -> Double -> ThreeScene Object3D
perspectiveCamera fo n fa = createCamera (PerspectiveCamera {fov = fo, near = n, far = fa})

orthographicCamera :: Double -> Double -> ThreeScene Object3D
orthographicCamera n f = createCamera (OrthographicCamera {near = n, far = f})
