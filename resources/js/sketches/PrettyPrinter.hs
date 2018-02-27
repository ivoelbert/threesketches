module PrettyPrinter where

import Text.PrettyPrint.HughesPJ
import ThreeTypes
import Data.List

printToFile :: ThreeScene () -> IO ()
printToFile scn = let pp = checkIntegrity (getSceneList scn)
                  in case pp of
                       Left err -> putStr err
                       Right str -> do putStr "success!\n"
                                       prelude <- readFile "threePrelude.js"
                                       postlude <- readFile "threePostlude.js"
                                       writeFile "SKETCH.js" $ prelude ++ str ++ postlude

checkIntegrity :: [(String, Object3D)] -> Either String String
checkIntegrity sceneList = let cameras = countCameras sceneList
                           in if cameras == 1
                              then case duplicates sceneList of
                                     Just name -> Left $ "Name \"" ++ name ++ "\" corresponds to multiple objects!\n"
                                     Nothing -> Right $ render $ pp sceneList
                              else if cameras < 1
                                   then Left "No camera to render the scene!\n"
                                   else Left "You can't have multiple cameras in the same scene!\n"

duplicates :: [(String, Object3D)] -> Maybe String
duplicates list = findDuplicate (Prelude.map Prelude.fst list)

count :: Eq a => a -> [a] -> Int
count e [] = 0
count e (a:xs) = (count e xs +) $ if a == e then 1 else 0

findDuplicate :: [String] -> Maybe String
findDuplicate [] = Nothing
findDuplicate (x:xs) = if count x xs > 0 then Just x else findDuplicate xs

countCameras :: [(String, Object3D)] -> Int
countCameras [] = 0
countCameras ((_, ThreeCamera _):xs) = countCameras xs + 1
countCameras (_:xs) = countCameras xs

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
                                 text (name ++ "Mesh.name = \"" ++ name ++ "\";\n") <>
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
{--
let nameLight = new THREE.AmbientLight( c, i );
scene.add( amblight );
--}
printLight name (AmbientLight {lightColor = c, intensity = i}) =
    text ("let " ++ name ++ "Light = new THREE.AmbientLight( ") <>
    printColor c <>
    text (", " ++ (show i) ++ " );\n") <>
    text (name ++ "Light.name = \"" ++ name ++ "\";\n") <>
    text ("scene.add( " ++ name ++ "Light );\n")

printLight name (DirectionalLight {lightColor = c, intensity = i, target = t}) =
    text ("let " ++ name ++ "Light = new THREE.DirectionalLight( ") <>
    printColor c <>
    text (", " ++ (show i) ++ " );\n") <>
    text (name ++ "Light.name = \"" ++ name ++ "\";\n") <>
    text ("scene.add( " ++ name ++ "Light );\n\n") <>
    text ("let " ++ name ++ "Target = new THREE.Object3D();\n") <>
    text (name ++ "Target.position.set( " ++ (show (vx t)) ++ ", " ++ (show (vy t)) ++ ", " ++ (show (vy t)) ++ " );\n") <>
    text ("scene.add( " ++ name ++ "Target );\n") <>
    text (name ++ "Light.target = " ++ name ++ "Target;\n")

printLight name (PointLight {lightColor = c, intensity = i, distance = dist, decay = dec}) =
    text ("let " ++ name ++ "Light = new THREE.PointLight( ") <>
    printColor c <>
    text (", " ++ (show i)) <>
    text (", " ++ (show dist)) <>
    text (", " ++ (show dec) ++ " );\n") <>
    text (name ++ "Light.name = " ++ name ++ ";\n") <>
    text ("scene.add( " ++ name ++ "Light );\n")

printCamera :: String -> Camera -> Doc
{--
let nameCamera = new THREE.PerspectiveCamera( fov, window.innerWidth/window.innerHeight, near, far );
--}
printCamera name (PerspectiveCamera {fov = fo, near = n, far = fa}) =
    text ("let camera = new THREE.PerspectiveCamera( ") <>
    text (show fo) <>
    text (", window.innerWidth/window.innerHeight") <>
    text (", " ++ (show n)) <>
    text (", " ++ (show fa) ++ " );\n") <>
    text ("camera.name = \"" ++ name ++ "\";\n") <>
    text ("scene.add( camera );\n")

{--
let nameCamera = new THREE.OrthographicCamera( window.innerWidth / - 2, window.innerWidth / 2, window.innerHeight / 2, window.innerHeight / - 2, 1, 1000 );
--}
printCamera name (OrthographicCamera {near = n, far = f}) =
    text ("let camera = new THREE.OrthographicCamera( ") <>
    text ("window.innerWidth / - 2, window.innerWidth / 2, window.innerHeight / 2, window.innerHeight / - 2") <>
    text (", " ++ (show n)) <>
    text (", " ++ (show f) ++ " );\n") <>
    text ("camera.name = \"" ++ name ++ "\";\n") <>
    text ("scene.add( camera );\n")
