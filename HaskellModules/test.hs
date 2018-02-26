import HaskellModules.ThreeTypes
import HaskellModules.PrettyPrinter

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
