const frameHelper = require('./resources/frameHelper.js');
const mouseHelper = require('./resources/mouseHelper.js');
const commonFunctions = require('./resources/commonFunctions.js');
const THREE = require('three');

const width = 800, height = 800;
const recording = false;
const animationFrames = 300;
let scene = new THREE.Scene();
let camera = new THREE.PerspectiveCamera( 70, width / height, 0.01, 3000 );

const cameraRad = 500;

// SETUP
const init = () => {
    frameHelper.resize(width, height);
    frameHelper.setFrameRate(30);
    
    camera.position.z = cameraRad;

    const dirLightTop = new THREE.DirectionalLight( 0x42bcf4, 0.5 );
    dirLightTop.position.set(0, 1, 0);
    scene.add( dirLightTop );

    const dirLightBottom = new THREE.DirectionalLight( 0xf2a32e, 0.5 );
    dirLightBottom.position.set(0, -1, 0);
    scene.add( dirLightBottom );

    const pointLight = new THREE.PointLight( 0xfafafa, 1, 500 );
    scene.add(pointLight);

    const fogColor = new THREE.Color(0x000000);
    scene.background = fogColor;
    scene.fog = new THREE.FogExp2(fogColor, 1 / 1000);

    const sphGeom = new THREE.SphereBufferGeometry(15, 32, 32);
    const sphMat = new THREE.MeshPhongMaterial({
        color: 0xfffdd3,
        shininess: 100
    });

    [...new Array(500)].map( _ => {
        const sph = new THREE.Mesh(sphGeom, sphMat);
        sph.position.copy(commonFunctions.randomVector().multiplyScalar(250));
        scene.add(sph);
    })

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