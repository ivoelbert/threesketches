const frameHelper = require('./resources/frameHelper.js');
const mouseHelper = require('./resources/mouseHelper.js');
const commonFunctions = require('./resources/commonFunctions.js');
const THREE = require('three');

const width = 800, height = 800;
const recording = false;
const animationFrames = 300;
let scene = new THREE.Scene();
let camera = new THREE.PerspectiveCamera( 40, width / height, 0.01, 3000 );
const cameraRad = 500;

let cubes;

// SETUP
const init = () => {
    frameHelper.recording(recording);
    frameHelper.resize(width, height);
    frameHelper.setFrameRate(30);

    camera.position.set(0, 0, cameraRad);
    camera.lookAt(0, 0, 0);

    const dirLightTop = new THREE.DirectionalLight( 0x8ddbff, 1 );
    dirLightTop.position.set(1, 1, 1);
    scene.add( dirLightTop );

    const dirLightBottom = new THREE.DirectionalLight( 0xf49538, 1 );
    dirLightBottom.position.set(-1, -1, -1);
    scene.add( dirLightBottom );

    const ambient = new THREE.AmbientLight(0xfafafa, 0.2);
    scene.add(ambient);

    const cubeGeom = new THREE.BoxBufferGeometry();
    const cubeMat = new THREE.MeshLambertMaterial({color: 0xfafafa});

    cubes = commonFunctions.tabulate(20, i => {
        const cube = new THREE.Mesh(cubeGeom, cubeMat);
        cube.scale.multiplyScalar(commonFunctions.randBetween(10, 100));
        cube.position.copy(commonFunctions.randomVector().multiplyScalar(100));
        scene.add(cube);
        return cube;
    })
    
    animate();
}

// UPDATE
const animate = () => {
    frameHelper.requestNextFrame( animate );

    const t = THREE.Math.mapLinear(frameHelper.frameCount, 0, animationFrames, 0, 1);

    const ang = t * 2 * Math.PI;

    cubes.map(cube => {
        cube.rotation.y = ang * cube.userData;
    })

    frameHelper.render(scene, camera);

    if(recording && frameHelper.frameCount < animationFrames)
        frameHelper.saveFrame("pendulum", frameHelper.frameCount);
};


// INIT 
init();
