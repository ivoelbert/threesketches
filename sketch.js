const frameHelper = require('./resources/frameHelper.js');
const mouseHelper = require('./resources/mouseHelper.js');
const utils = require('./resources/utils.js');
const THREE = require('three');

const width = 600, height = 600;
const recording = false;
const animationFrames = 210;
let scene = new THREE.Scene();
let camera = new THREE.PerspectiveCamera( 60, width / height, 0.01, 3000 );
const cameraRad = 500;

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

    scene.background = new THREE.Color(0xfafafa);

    const geom = new THREE.SphereBufferGeometry(1, 32, 32);
    const mat = new THREE.MeshPhongMaterial({
        color: 0xfafafa,
        shininess: 90,
    });

    const collides = (sph1, sph2) => {
        const dist = sph1.position.clone().sub(sph2.position);
        return dist.length() < sph1.scale.x + sph2.scale.x;
    }

    const randomSphere = () => {
        const mesh = new THREE.Mesh(geom, mat);
        mesh.scale.multiplyScalar(utils.randBetween(10, 50));
        const pos = utils.randomVector().multiplyScalar(utils.randBetween(-150, 150));
        mesh.position.copy(pos);
        return mesh;
    }

    let sphs = [randomSphere()];
    utils.repeat(11, i => {
        const sph = utils.doWhile( () => {
            const sph = randomSphere();
            const collision = sphs.reduce( (acc, curr) => !collides(curr, sph) && acc, true);
            return collision ? sph : null;
        });
        sphs.push(sph);
        scene.add(sph);
    });

    animate();
}

// UPDATE
const animate = () => {
    frameHelper.requestNextFrame( animate );

    const t = THREE.Math.mapLinear(frameHelper.frameCount, 0, animationFrames, 0, 1);

    const ang = t * 2 * Math.PI;
    const camx = Math.cos(ang) * cameraRad;
    const camz = Math.sin(ang) * cameraRad;
    camera.position.set(camx, 0, camz);
    camera.lookAt(0, 0, 0);

    frameHelper.render(scene, camera);

    if(recording && frameHelper.frameCount < animationFrames)
        frameHelper.saveFrame("esferas", frameHelper.frameCount);
};


// INIT 
init();

