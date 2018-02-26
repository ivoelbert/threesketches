module HaskellModules.PrettyPrinter where

import Text.PrettyPrint.HughesPJ
import HaskellModules.ThreeTypes


printToFile :: ThreeScene () -> IO ()
printToFile scn = let str = prettyp scn in writeFile "test.js" str

prettyp :: ThreeScene () -> String
prettyp scn = render $ pp (getSceneList scn)


pp :: [(String, Object3D)] -> Doc
pp [] = empty
pp (x:xs) = printObj x <>
            text "\n" <>
            pp xs


printObj :: (String, Object3D) -> Doc
printObj (name, ThreeMesh mesh) = printMesh name mesh
printObj (name, ThreeLight light) = printLight name light
printObj (name, ThreeCamera camera) = printCamera name camera


printMesh :: String -> Mesh -> Doc
printMesh name (Mesh geom mat) = (printGeometry name geom) <>
                                 text "\n" <>
                                 (printMaterial name mat) <>
                                 text "\n" <>
                                 text ("let " ++ name ++ "Mesh = new THREE.Mesh( " ++
                                 name ++ "Geometry, " ++
                                 name ++ "Material );") <>
                                 text "\n" <>
                                 text ("scene.add( " ++ name ++ "Mesh );\n")



printGeometry :: String -> Geometry -> Doc
--let nameGeometry = new THREE.BoxBufferGeometry( width, height, depth );
printGeometry name (BoxGeometry {width = w, height = h, depth = d}) =
    text ("let " ++ name ++ "Geometry = new THREE.BoxBufferGeometry( " ++
    (show w) ++ ", " ++
    (show h) ++ ", " ++
    (show d) ++
    " );")

--let nameGeometry = new THREE.ConeBufferGeometry( radius, height, radialSegments );
printGeometry name (ConeGeometry {radius = r, height = h, radialSegments = rs}) =
    text ("let " ++ name ++ "Geometry = new THREE.ConeBufferGeometry( " ++
    (show r) ++ ", " ++
    (show h) ++ ", " ++
    (show rs) ++
    " );")

--let nameGeometry = new THREE.CylinderBufferGeometry( radiusTop, radiusBottom, height, radialSegments );
printGeometry name (CylinderGeometry {radiusTop = rt, radiusBottom = rb, height = h, radialSegments = rs}) =
    text ("let " ++ name ++ "Geometry = new THREE.CylinderBufferGeometry( " ++
    (show rt) ++ ", " ++
    (show rb) ++ ", " ++
    (show h) ++ ", " ++
    (show rs) ++
    " );")

--let nameGeometry = new THREE.SphereBufferGeometry( radius, widthSegments, heightSegments );
printGeometry name (SphereGeometry {radius = r, widthSegments = ws, heightSegments = hs}) =
    text ("let " ++ name ++ "Geometry = new THREE.SphereBufferGeometry( " ++
    (show r) ++ ", " ++
    (show ws) ++ ", " ++
    (show hs) ++
    " );")

--let nameGeometry = new THREE.TorusBufferGeometry( radius, tube, radialSegments, tubularSegments );
printGeometry name (TorusGeometry {radius = r, tube = t, radialSegments = rs, tubularSegments = ts}) =
    text ("let " ++ name ++ "Geometry = new THREE.TorusBufferGeometry( " ++
    (show r) ++ ", " ++
    (show t) ++ ", " ++
    (show rs) ++ ", " ++
    (show ts) ++
    " );")


printMaterial :: String -> Material -> Doc
{--
let nameMaterial = new THREE.MeshStandardMaterial( {
                                                        color: materialColor,
                                                        emissive: emissive,
                                                        roughness: roughness,
                                                        metalness: metalness
                                                   } );
--}
printMaterial name (StandardMaterial {materialColor = c, emissive = e, roughness = r, metalness = m}) =
    text ("let " ++ name ++ "Material = new THREE.MeshStandardMaterial( {\n") <>
    text "    color: " <> (printColor c) <> text ",\n" <>
    text "    emissive: " <> (printColor e) <> text ",\n" <>
    text ("    roughness: " ++ (show r) ++ ",\n") <>
    text ("    metalness: " ++ (show m) ++ "\n") <>
    text ("} );")


printColor :: Color -> Doc
printColor (RGB r g b) = text ("new THREE.Color(" ++ (show r) ++ ", " ++ (show g) ++ ", " ++ (show b) ++ ")")
printColor (HEX s) = text ("0x" ++ s)

printLight :: String -> Light -> Doc
printLight name _ = empty

printCamera :: String -> Camera -> Doc
printCamera name _ = empty
