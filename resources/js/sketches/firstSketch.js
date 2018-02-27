var scene = new THREE.Scene();

var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true, antialias: true } );
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

let luzLight = new THREE.DirectionalLight( 0xffffff, 1.0 );
scene.add( luzLight );

let luzTarget = new THREE.Object3D();
luzTarget.position.set( 0.0, 0.0, 0.0 );
scene.add( luzTarget );
luzLight.target = luzTarget;

let camera = new THREE.PerspectiveCamera( 50.0, window.innerWidth/window.innerHeight, 0.1, 100.0 );
camera.position.z = 5
scene.add( camera );

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



var animate = function () {

  requestNextFrame( animate );

	renderer.render(scene, camera);

};

setFrameRate(30);
animate();
