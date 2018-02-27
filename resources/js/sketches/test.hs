--{-# LANGUAGE OverloadedStrings #-}
--import Haste.Foreign
--import Haste.Prim (toJSStr)

import ThreeTypes
import PrettyPrinter
--import Update

scene :: ThreeScene ()
scene = do
        newScene
        cubeGeometry <- createGeometry (BoxGeometry {width = 1.5, height = 3, depth = 1.5})
        cubeMaterial <- createMaterial (StandardMaterial {materialColor = HEX "aaaaaa", emissive = HEX "000000", roughness = 1, metalness = 0 })
        cubeMesh <- createMesh cubeGeometry cubeMaterial
        addToScene "cubo" cubeMesh
        sphereGeometry <- createGeometry (SphereGeometry {radius = 1, widthSegments = 32, heightSegments = 32})
        sphereMaterial <- createMaterial (StandardMaterial {materialColor = RGB 0.8 0.2 0, emissive = RGB 0 0 0, roughness = 0.5, metalness = 1})
        sphereMesh <- createMesh sphereGeometry sphereMaterial
        addToScene "esfera" sphereMesh
        camera <- createCamera (PerspectiveCamera {fov = 75, near = 0.1, far = 100})
        addToScene "camara" camera
        luz <- createLight (DirectionalLight {lightColor = HEX "ffffff", intensity = 1, target = (vector 0 0 0)})
        addToScene "luz" luz
        cubeGeometry2 <- createGeometry (BoxGeometry {width = 3, height = 1, depth = 1})
        cubeMaterial2 <- createMaterial (StandardMaterial {materialColor = HEX "aaaaaa", emissive = HEX "000000", roughness = 1, metalness = 1 })
        cubeMesh2 <- createMesh cubeGeometry2 cubeMaterial2
        addToScene "cubo2" cubeMesh2

{--
update :: Double -> ThreeAnimation ()
update frame = do
               newAnimation
               translate "esfera" (vector 0 0 (-1)) 0.1
               translate "cubo" (vector 0 0 (-1)) 0.1
               setPosition "camara" (vector 0 (frame * 0.1) 5)
               setPosition "luz" (vector 0 0 5)

updates = createUpdate update

main = do
       export "updates" updates
--}
