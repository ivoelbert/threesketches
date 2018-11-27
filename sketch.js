const frameHelper = require('./resources/frameHelper.js');
const mouseHelper = require('./resources/mouseHelper.js');
const commonFunctions = require('./resources/commonFunctions.js');
const THREE = require('three');

const recording = false;
const animationFrames = 300;
let scene = new THREE.Scene();
let camera = new THREE.PerspectiveCamera( 70, window.innerWidth / window.innerHeight, 0.01, 3000 );

const cameraRad = 500;

// SETUP
const init = () => {
    frameHelper.setFrameRate(30);
    
    camera.position.z = cameraRad;

    const fogColor = new THREE.Color(0x000000);
    scene.background = fogColor;
    scene.fog = new THREE.FogExp2(fogColor, 1 / 1000);

    animate();
}

// UPDATE
const animate = () => {
    frameHelper.requestNextFrame( animate );

    const t = THREE.Math.mapLinear(frameHelper.frameCount, 0, animationFrames, 0, 1);
    const ang = t * 2 * Math.PI;
    camera.position.set(Math.sin(ang) * cameraRad, 0, Math.cos(ang) * cameraRad);
    camera.lookAt(0, 0, 0);

    frameHelper.render(scene, camera);

    if(recording && frameHelper.frameCount < animationFrames)
        frameHelper.saveFrame("bubbles", frameHelper.frameCount);
};



// INIT 
init();