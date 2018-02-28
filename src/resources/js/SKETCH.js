var scene = new THREE.Scene();

var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true, antialias: true } );
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

////////////////////////////////////////////////////////
// BEGIN PRINTED FROM HASKELL
////////////////////////////////////////////////////////
 
let bolaGeometry = new THREE.SphereBufferGeometry( 0.5, 32, 32 );
let bolaMaterial = new THREE.MeshStandardMaterial( {
    color: 0xffffff,
    emissive: 0x111111,
    roughness: 1.0,
    metalness: 0.5
} );
let bolaMesh = new THREE.Mesh( bolaGeometry, bolaMaterial );
bolaMesh.name = "bola";
scene.add( bolaMesh );

let mesaGeometry = new THREE.BoxBufferGeometry( 5.0, 1.0, 5.0 );
let mesaMaterial = new THREE.MeshStandardMaterial( {
    color: 0x47aa68,
    emissive: 0x000000,
    roughness: 0.8,
    metalness: 0.1
} );
let mesaMesh = new THREE.Mesh( mesaGeometry, mesaMaterial );
mesaMesh.name = "mesa";
scene.add( mesaMesh );

let ambLightLight = new THREE.AmbientLight( 0xf9e0a9, 0.5 );
ambLightLight.name = "ambLight";
scene.add( ambLightLight );

let dirLightLight = new THREE.DirectionalLight( 0xffffff, 1.0 );
dirLightLight.name = "dirLight";
scene.add( dirLightLight );

let dirLightTarget = new THREE.Object3D();
dirLightTarget.position.set( 0.0, 0.0, 0.0 );
scene.add( dirLightTarget );
dirLightLight.target = dirLightTarget;

let camera = new THREE.PerspectiveCamera( 75.0, window.innerWidth/window.innerHeight, 0.1, 100.0 );
camera.name = "camara";
scene.add( camera );

////////////////////////////////////////////////////////
// END PRINTED FROM HASKELL
////////////////////////////////////////////////////////

var animate = function () {

  requestNextFrame( animate );
  handleUpdates(Haste.updates(frameCount));
	renderer.render(scene, camera);

};

setFrameRate(30);
animate();
