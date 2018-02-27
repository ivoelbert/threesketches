var scene = new THREE.Scene();

var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true, antialias: true } );
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

////////////////////////////////////////////////////////
// BEGIN PRINTED FROM HASKELL
////////////////////////////////////////////////////////
 
let cubo2Geometry = new THREE.BoxBufferGeometry( 3.0, 1.0, 1.0 );
let cubo2Material = new THREE.MeshStandardMaterial( {
    color: 0xaaaaaa,
    emissive: 0x000000,
    roughness: 1.0,
    metalness: 1.0
} );
let cubo2Mesh = new THREE.Mesh( cubo2Geometry, cubo2Material );
cubo2Mesh.name = "cubo2";
scene.add( cubo2Mesh );

let luzLight = new THREE.DirectionalLight( 0xffffff, 1.0 );
luzLight.name = "luz";
scene.add( luzLight );

let luzTarget = new THREE.Object3D();
luzTarget.position.set( 0.0, 0.0, 0.0 );
scene.add( luzTarget );
luzLight.target = luzTarget;

let camera = new THREE.PerspectiveCamera( 75.0, window.innerWidth/window.innerHeight, 0.1, 100.0 );
camera.name = "camara";
scene.add( camera );

let esferaGeometry = new THREE.SphereBufferGeometry( 1.0, 32, 32 );
let esferaMaterial = new THREE.MeshStandardMaterial( {
    color: new THREE.Color(0.8, 0.2, 0.0),
    emissive: new THREE.Color(0.0, 0.0, 0.0),
    roughness: 0.5,
    metalness: 1.0
} );
let esferaMesh = new THREE.Mesh( esferaGeometry, esferaMaterial );
esferaMesh.name = "esfera";
scene.add( esferaMesh );

let cuboGeometry = new THREE.BoxBufferGeometry( 1.5, 3.0, 1.5 );
let cuboMaterial = new THREE.MeshStandardMaterial( {
    color: 0xaaaaaa,
    emissive: 0x000000,
    roughness: 1.0,
    metalness: 0.0
} );
let cuboMesh = new THREE.Mesh( cuboGeometry, cuboMaterial );
cuboMesh.name = "cubo";
scene.add( cuboMesh );

////////////////////////////////////////////////////////
// END PRINTED FROM HASKELL
////////////////////////////////////////////////////////

var animate = function () {

  requestNextFrame( animate );
  console.log(Haste.updates(frameCount));
	renderer.render(scene, camera);

};

setFrameRate(30);
animate();
