var scene = new THREE.Scene();
scene.background = new THREE.Color( 0x222222 );

var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );
camera.position.z = 30;

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

/////////////// SORETE ///////////////
// Textura de metal
var metalText = new THREE.TextureLoader().load( "resources/textures/metaltexture.jpg" );
metalText.wrapS = THREE.MirroredRepeatWrapping;
metalText.wrapT = THREE.MirroredRepeatWrapping;
metalText.repeat.set( 30, 1 );

function CustomSinCurve( scale ) {

  THREE.Curve.call( this );

  this.scale = ( scale === undefined ) ? 1 : scale;

}

CustomSinCurve.prototype = Object.create( THREE.Curve.prototype );
CustomSinCurve.prototype.constructor = CustomSinCurve;

CustomSinCurve.prototype.getPoint = function ( t ) {

  let sqr = t * t * 0.9;
  let rad = THREE.Math.mapLinear(sqr, 0, 1, 0.8, 0);
  var tx = Math.sin( 8 * Math.PI * t ) * rad;
  var ty = t * t * 0.4 + t * 0.8;
  var tz = Math.cos( 8 * Math.PI * t ) * rad;

  return new THREE.Vector3( tx, ty, tz ).multiplyScalar( this.scale );

};

let path = new CustomSinCurve( 10 );

// Geometria
let soreteGeometry = new THREE.TubeGeometry(path, 128, 2, 32, false);

// Material
let soreteMaterial = new THREE.MeshStandardMaterial( {
                                                        bumpMap: metalText,
                                                        bumpScale: 1,
                                                        roughness: 1,
                                                        metalness: 0,
                                                        color: 0x3d240f
                                                   } );
let sorete = new THREE.Mesh( soreteGeometry, soreteMaterial );
sorete.position.set(0, -6, 0);
sorete.rotation.y = Math.PI / 2 + 0.2;
sorete.rotation.x = 0.1;
scene.add( sorete );
///////////////////////////////////////////




// Ojos
let ojoGeometry = new THREE.SphereGeometry( 1.9, 32, 32 );
let ojoMaterial = new THREE.MeshStandardMaterial( {
                                                    roughness: 1,
                                                    metalness: 0,
                                                    color: 0xcccccc
                                                } );

let pupilaGeometry = new THREE.SphereGeometry( 0.8, 32, 32 );
let pupilaMaterial = new THREE.MeshPhongMaterial( {
                                                    shininess: 90,
                                                    color: 0x101010
                                                } );

let eyeSeparation = 2.3;
let eyeDistance = 10;

let ojoizq = new THREE.Mesh(ojoGeometry, ojoMaterial);
ojoizq.position.set(-eyeSeparation, -1, eyeDistance);
scene.add(ojoizq);

let ojoder = new THREE.Mesh(ojoGeometry, ojoMaterial);
ojoder.position.set(eyeSeparation, -1, eyeDistance);
scene.add(ojoder);

let pupilaizq = new THREE.Mesh(pupilaGeometry, pupilaMaterial);
pupilaizq.position.set(0, 0, 1.5);
ojoizq.add(pupilaizq);


let pupilader = new THREE.Mesh(pupilaGeometry, pupilaMaterial);
pupilader.position.set(0, 0, 1.5);
ojoder.add(pupilader);



var animate = function () {

  requestNextFrame( animate );
  
  let ang = THREE.Math.mapLinear(frameCount, 0, 120, 0, Math.PI * 2);
  let rot = THREE.Math.mapLinear(Math.sin(ang), -1, 1, -0.05, 0.05);
  let cameraRad = 30;

  let px = Math.sin(rot) * cameraRad;
  let py = 0;
  let pz = Math.cos(rot) * cameraRad;

  camera.position.set(px, py, pz);
  camera.up.set(0, 1, 0);
  camera.lookAt(new THREE.Vector3(0, 0, 0));

  //let mouse = new THREE.Vector3(mouseX, mouseY, 100);

  let mouseSens = 30;
  ojoizq.up.set(0, 1, 0);
  ojoizq.lookAt(new THREE.Vector3(mouseX * mouseSens, mouseY * mouseSens, 100));
  ojoder.up.set(0, 1, 0);
  ojoder.lookAt(new THREE.Vector3(mouseX * mouseSens, mouseY * mouseSens, 100));

	renderer.render(scene, camera);

  /*
  if(frameCount < 300)
  {
    saveFrame("test", frameCount);
  }
  */

};

setFrameRate(30);
animate();
