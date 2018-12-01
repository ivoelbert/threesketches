const frameHelper = require('./resources/frameHelper.js');
const mouseHelper = require('./resources/mouseHelper.js');
const commonFunctions = require('./resources/commonFunctions.js');
const THREE = require('three');

const width = 800, height = 800;
const recording = false;
const animationFrames = 300;
let scene = new THREE.Scene();
let camera = new THREE.PerspectiveCamera( 60, width / height, 0.01, 3000 );
const cameraRad = 500;

let cubes;
const lado = 300;

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

    fogColor = new THREE.Color(0x000000);
 
    scene.background = fogColor;
    scene.fog = new THREE.FogExp2(fogColor, 1 / 1500);



    const angSum = (w, h, d) => {
        const vec = new THREE.Vector3(w, h, d);
        const xToPlane = (Math.PI / 2) - vec.angleTo(new THREE.Vector3(1, 0, 0));
        const yToPlane = (Math.PI / 2) - vec.angleTo(new THREE.Vector3(0, 1, 0));
        const zToPlane = (Math.PI / 2) - vec.angleTo(new THREE.Vector3(0, 0, 1));
        return THREE.Math.radToDeg(xToPlane + yToPlane + zToPlane);
    }

    const sphGeom = new THREE.SphereBufferGeometry(1, 32, 32);
    const muestras = 10;
    commonFunctions.repeat(muestras, x => {
        commonFunctions.repeat(muestras, y => {
            commonFunctions.repeat(muestras, z => {
                const px = THREE.Math.mapLinear(x, 0, muestras, -lado/2, lado/2);
                const py = THREE.Math.mapLinear(y, 0, muestras, -lado/2, lado/2);
                const pz = THREE.Math.mapLinear(z, 0, muestras, -lado/2, lado/2);

                const sum = THREE.Math.mapLinear(angSum(x + 1, y + 1, z + 1), 90, 106, 0, 1);

                const h = THREE.Math.mapLinear(sum, 0, 1, 0, 0.7);
                const l = THREE.Math.mapLinear(sum, 0, 1, 0.1, 0.9);
                const col = new THREE.Color().setHSL(h, 0.9, l);
                const sphMat = new THREE.MeshLambertMaterial({color: col, transparent: true});
                const opac = THREE.Math.mapLinear(sum, 0, 1, 0, 0.75);
                sphMat.opacity = opac;
                const sph = new THREE.Mesh(sphGeom, sphMat);
                
                const scl = THREE.Math.mapLinear(sum, 0, 1, 0.1, 30);
                sph.scale.setLength(scl);
                sph.position.set(px, py, pz);

                scene.add(sph);
            })
        })  
    })
    
    animate();
}

// UPDATE
const animate = () => {
    frameHelper.requestNextFrame( animate );

    const t = THREE.Math.mapLinear(frameHelper.frameCount, 0, animationFrames, 0, 1);

    const ang = THREE.Math.mapLinear(mouseHelper.mouseX, -1, 1, -Math.PI, Math.PI);
    const angY = THREE.Math.mapLinear(mouseHelper.mouseY, -1, 1, -Math.PI, Math.PI);

    const camx = Math.cos(ang) * cameraRad;
    const camz = Math.sin(ang) * cameraRad;
    const camy = Math.sin(angY) * cameraRad;
    camera.position.set(camx, camy, camz);
    camera.lookAt(0, 0, 0);

    frameHelper.render(scene, camera);

    if(recording && frameHelper.frameCount < animationFrames)
        frameHelper.saveFrame("pendulum", frameHelper.frameCount);
};


// INIT 
init();
