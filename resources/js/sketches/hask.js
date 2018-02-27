var scene = new THREE.Scene();
scene.background = new THREE.Color( 0x000000 );

var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );
camera.position.z = 4;

var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true, antialias: true } );
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

// Luz ambiente
var amblight = new THREE.AmbientLight( 0x888888 );
scene.add( amblight );

// Luz direccional
var directionalLight = new THREE.DirectionalLight( 0xffffff, 1 );
directionalLight.position.set(-20, 100, 100)
scene.add( directionalLight );

// Luz direccional
var directionalLight2 = new THREE.DirectionalLight( 0x444444, 1 );
directionalLight2.position.set(20, -100, 100)
scene.add( directionalLight2 );


/////////////////////////////////////////
let esferaGeometry = new THREE.SphereBufferGeometry( 1.0, 32, 32 );
let esferaMaterial = new THREE.MeshStandardMaterial( {
    color: new THREE.Color(0.8, 0.2, 0.0),
    emissive: new THREE.Color(0.0, 0.0, 0.0),
    roughness: 0.5,
    metalness: 1.0
} );
let esferaMesh = new THREE.Mesh( esferaGeometry, esferaMaterial );
scene.add( esferaMesh );

let cuboGeometry = new THREE.BoxBufferGeometry( 1.5, 3.0, 1.5 );
let cuboMaterial = new THREE.MeshStandardMaterial( {
    color: 0xaaaaaa,
    emissive: 0x000000,
    roughness: 1.0,
    metalness: 0.0
} );
let cuboMesh = new THREE.Mesh( cuboGeometry, cuboMaterial );
scene.add( cuboMesh );
/////////////////////////////////////////


var setup = function() {
  setFrameRate(30);
  animate();
}

var animate = function () {

  requestNextFrame( animate );

  let ang = THREE.Math.mapLinear(frameCount, 0, 120, 0, Math.PI * 2);
  let rot = THREE.Math.mapLinear(Math.sin(ang), -1, 1, -0.05, 0.05);
  let cameraRad = 4;

  let px = Math.sin(rot) * cameraRad;
  let py = 0;
  let pz = Math.cos(rot) * cameraRad;

  camera.position.set(px, py, pz);
  camera.up.set(0, 1, 0);
  camera.lookAt(new THREE.Vector3(0, 0, 0));

  console.log(Haste.updates(frameCount));

	renderer.render(scene, camera);

};

setup();
