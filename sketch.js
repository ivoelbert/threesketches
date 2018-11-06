const frameHelper = require('./resources/frameHelper.js');
const mouseHelper = require('./resources/mouseHelper.js');
const THREE = require('three');

let scene = new THREE.Scene();
let camera = new THREE.PerspectiveCamera( 70, window.innerWidth / window.innerHeight, 0.01, 10 );

let mesh;

const init = () => {
    frameHelper.setFrameRate(30);
    
    camera.position.z = 1;

    const geometry = new THREE.BoxGeometry( 0.2, 0.2, 0.2 );
	const material = new THREE.MeshNormalMaterial();

	mesh = new THREE.Mesh( geometry, material );
	scene.add( mesh );

    animate();
}

const animate = () => {
    frameHelper.requestNextFrame( animate );
    
    mesh.rotation.x += 0.01;
    mesh.rotation.y = mouseHelper.mouseX;
    
    frameHelper.render(scene, camera);

    if(frameHelper.frameCount < 10)
        frameHelper.saveFrame("test", frameHelper.frameCount);
};

init();