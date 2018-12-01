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

let tubes = [];

// SETUP
const init = () => {
    frameHelper.recording(recording);
    frameHelper.resize(width, height);
    frameHelper.setFrameRate(30);

    camera.position.set(0, 200, cameraRad);
    camera.lookAt(0, 0, 0);

    const dirLightTop = new THREE.DirectionalLight( 0xfff7c4, 1 );
    dirLightTop.position.set(1, 1, 1);
    scene.add( dirLightTop );

    const dirLightBottom = new THREE.DirectionalLight( 0xf2a32e, 1 );
    dirLightBottom.position.set(-1, -1, -1);
    scene.add( dirLightBottom );

    const ambient = new THREE.AmbientLight(0xfafafa, 0.5);
    scene.add(ambient);

    const fogColor = new THREE.Color(0xfffce5);
    scene.background = fogColor;
    scene.fog = new THREE.FogExp2(fogColor, 1 / 1000);

    const lado = 200;
    const cantPorLado = 20;
    tubes = commonFunctions.tabulate(cantPorLado * cantPorLado, i => {
        const x = i % cantPorLado;
        const z = Math.floor(i / cantPorLado);
        const cylMat = new THREE.MeshLambertMaterial({color: 0x147f36});
        const tubo = createTube(4, 500, cylMat);
        tubo.position.x = THREE.Math.mapLinear(x, 0, cantPorLado - 1, -lado/2, lado/2);
        tubo.position.z = THREE.Math.mapLinear(z, 0, cantPorLado - 1, -lado/2, lado/2);
        tubo.position.y -= 50;
        scene.add(tubo);
        return tubo;
    });

    (() => {
        const baseLado = lado + 10;
        let shape = new THREE.Shape();
        shape.moveTo( -baseLado/2, -baseLado/2 );
        shape.lineTo( -baseLado/2, baseLado/2 );
        shape.lineTo( baseLado/2, baseLado/2 );
        shape.lineTo( baseLado/2, -baseLado/2 );
        shape.lineTo( -baseLado/2, -baseLado/2 );
    
        const extrudeSettings = {
            steps: 2,
            depth: 100,
            bevelEnabled: true,
            bevelThickness: 2,
            bevelSize: 2,
            bevelSegments: 3
        };
    
        const baseGeom = new THREE.ExtrudeBufferGeometry( shape, extrudeSettings );
        const baseMat = new THREE.MeshLambertMaterial({color: 0x383127});
        const base = new THREE.Mesh( baseGeom, baseMat );
        base.rotation.x = Math.PI / 2;
        base.position.y = -100;
        scene.add( base );
    })();

    (() => {
        const baseLado = lado + 100;
        let shape = new THREE.Shape();
        shape.moveTo( -baseLado/2, -baseLado/2 );
        shape.lineTo( -baseLado/2, baseLado/2 );
        shape.lineTo( baseLado/2, baseLado/2 );
        shape.lineTo( baseLado/2, -baseLado/2 );
        shape.lineTo( -baseLado/2, -baseLado/2 );
    
        const extrudeSettings = {
            steps: 2,
            depth: 100,
            bevelEnabled: true,
            bevelThickness: 2,
            bevelSize: 2,
            bevelSegments: 3
        };
    
        const baseGeom = new THREE.ExtrudeBufferGeometry( shape, extrudeSettings );
        const baseMat = new THREE.MeshLambertMaterial({color: 0x111010});
        const base = new THREE.Mesh( baseGeom, baseMat );
        base.rotation.x = Math.PI / 2;
        base.position.y = -110;
        scene.add( base );
    })();
    

    animate();
}

// UPDATE
const animate = () => {
    frameHelper.requestNextFrame( animate );

    const t = THREE.Math.mapLinear(frameHelper.frameCount, 0, animationFrames, 0, 1);

    frameHelper.render(scene, camera);

    if(recording && frameHelper.frameCount < animationFrames)
        frameHelper.saveFrame("pendulum", frameHelper.frameCount);
};


const createTube = (r, h, mat) => {
    const cylGeom = new THREE.CylinderBufferGeometry(r, r, h, 32, 1, true);
    const cyl = new THREE.Mesh(cylGeom, mat);
    cyl.position.y = -h / 2;
    const sphGeom = new THREE.SphereBufferGeometry(r, 32, 32, 0, Math.PI * 2, 0, Math.PI / 2);
    const sph = new THREE.Mesh(sphGeom, mat);
    sph.position.y = h / 2;
    cyl.add(sph);

    return cyl;
}


// INIT 
init();
