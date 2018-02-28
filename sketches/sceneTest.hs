import ThreeScene

scene :: ThreeScene ()
scene = do
        newScene
        cam <- perspectiveCamera 75 0.1 100
        addToScene "camara" cam
        luz1 <- directionalLight (hex "ffffff") 1 vec0
        luz2 <- ambientLight (hex "f9e0a9") 0.5
        addToScene "dirLight" luz1
        addToScene "ambLight" luz2
        tableGeom <- boxGeometry 5 1 5
        tableMat <- standardMaterial (hex "47aa68") (hex "000000") 0.8 0.1
        tableMesh <- createMesh tableGeom tableMat
        addToScene "mesa" tableMesh
        bolaGeom <- sphereGeometry 0.5 32 32
        bolaMat <- standardMaterial (hex "ffffff") (hex "111111") 1 0.5
        bolaMesh <- createMesh bolaGeom bolaMat
        addToScene "bola" bolaMesh

main :: IO ()
main = do printToFile scene
