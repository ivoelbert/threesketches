var scene = new THREE.Scene();

var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );
camera.position.z = 40;

var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true, antialias: true } );
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

// Luz ambiente
var amblight = new THREE.AmbientLight( 0x101010 );
scene.add( amblight );

// Luz puntual
var light = new THREE.PointLight( 0xffffff, 1, 1000 );
light.position.set( 0, 20, 20 );
scene.add( light );

/////////////// TORUS KNOT ///////////////
// Textura de metal
var metalText = new THREE.TextureLoader().load( "resources/textures/metaltexture.jpg" );
metalText.wrapS = THREE.MirroredRepeatWrapping;
metalText.wrapT = THREE.MirroredRepeatWrapping;
metalText.repeat.set( 40, 1 );

// Bump map (uso la misma textura)
var bmap = new THREE.TextureLoader().load( "resources/textures/metaltexture.jpg" );
bmap.wrapS = THREE.RepeatWrapping;
bmap.wrapT = THREE.RepeatWrapping;

// Geometria
var geometry = new THREE.TorusKnotGeometry( 12, 2.5, 400, 32, 3, 5 );

// Material
var material = new THREE.MeshPhongMaterial( {
                                              specular: 0xa4a29a,
                                              shininess: 70,
                                              bumpMap: metalText,
                                              bumpScale: 0.1,
                                              map: metalText
                                          } );
var torusKnot = new THREE.Mesh( geometry, material );
scene.add( torusKnot );
///////////////////////////////////////////

var animate = function () {

  requestNextFrame( animate );

  let rot = THREE.Math.mapLinear(frameCount, 0, 180, 0, Math.PI * 2);
	torusKnot.rotation.y = rot;

	renderer.render(scene, camera);

  /*
  if(frameCount < 181)
  {
    saveFrame("test", frameCount);
  }
  */

};

setFrameRate(30);
animate();
