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

const sphPerSide = 10;
const sphSz = 10;
const side = 400;

let sphs = [];
let pos = [];

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

    const sphGeom = new THREE.SphereBufferGeometry(sphSz, 32, 32);
    const sphMat = new THREE.MeshPhongMaterial({
        color: 0xfffdd3,
        shininess: 100
    });

    [...new Array(sphPerSide)].map( (_, x) => {
        [...new Array(sphPerSide)].map( (_, y) => {
            [...new Array(sphPerSide)].map( (_, z) => {
                const sph = new THREE.Mesh(sphGeom, sphMat);
                const px = THREE.Math.mapLinear(x, 0, sphPerSide, -side/2, side/2);
                const py = THREE.Math.mapLinear(y, 0, sphPerSide, -side/2, side/2);
                const pz = THREE.Math.mapLinear(z, 0, sphPerSide, -side/2, side/2);
                sph.position.set(px, py, pz);
                pos.push(new THREE.Vector3(px, py, pz));
                sphs.push(sph);
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
    const ang = t * 2 * Math.PI;
    camera.position.set(Math.sin(ang) * cameraRad, 0, Math.cos(ang) * cameraRad);
    camera.lookAt(0, 0, 0);

    const wiggleSz = 30;
    const wiggles = 2;
    const speed = 5;
    sphs.map( (sph, i) => {
        const {x, y, z} = sph.position;

        const normX = THREE.Math.mapLinear(x, -side/2, side/2, 0, Math.PI * 2 * wiggles);
        const normY = THREE.Math.mapLinear(y, -side/2, side/2, 0, Math.PI * 2 * wiggles);
        const normZ = THREE.Math.mapLinear(z, -side/2, side/2, 0, Math.PI * 2 * wiggles);
        
        const px = THREE.Math.mapLinear(Math.sin(normY + ang * speed) + Math.cos(normZ + ang * speed), -2, 2, -wiggleSz, wiggleSz);
        const py = THREE.Math.mapLinear(Math.sin(normX + ang * speed) + Math.cos(normZ + ang * speed), -2, 2, -wiggleSz, wiggleSz);
        const pz = THREE.Math.mapLinear(Math.sin(normX + ang * speed) + Math.cos(normY + ang * speed), -2, 2, -wiggleSz, wiggleSz);

        sph.position.set(pos[i].x + px, pos[i].y + py, pos[i].z + pz);
    });

    frameHelper.render(scene, camera);

    if(recording && frameHelper.frameCount < animationFrames)
        frameHelper.saveFrame("bubbles", frameHelper.frameCount);
};



// INIT 
init();