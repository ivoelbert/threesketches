const frameHelper = require('./resources/frameHelper.js');
const mouseHelper = require('./resources/mouseHelper.js');
const THREE = require('three');

const recording = true;
const animationFrames = 240;
const rad = 450;

let scene = new THREE.Scene();
let camera = new THREE.PerspectiveCamera( 70, window.innerWidth / window.innerHeight, 0.01, 3000 );
const cameraRad = 1000;

// SETUP
const init = () => {
    frameHelper.setFrameRate(30);
    
    camera.position.z = 1000;

    const colors = [...Array(10)].map( (v, index) => {
        let color = new THREE.Color();
        return color.setHSL( 0.125, 1.0, index / 10 );
    })

    colors.reverse().map( (col, index) => {
        const geometry = createLines(1000, THREE.Math.mapLinear(index+1, 0, colors.length, 1, rad), 1.2);
    
        const material = new THREE.LineBasicMaterial( { color: col, opacity: 0.1 } );
        const line = new THREE.LineSegments( geometry, material );
        scene.add(line);
    })
    

    animate();
}

// UPDATE
const animate = () => {
    frameHelper.requestNextFrame( animate );

    const ang = THREE.Math.mapLinear(frameHelper.frameCount, 0, animationFrames, 0, 2 * Math.PI);
    camera.position.set(Math.sin(ang) * cameraRad, 0, Math.cos(ang) * cameraRad);
    camera.lookAt(0, 0, 0);

    scene.children.map( obj => {
        if(obj.isLine) {
            setScale(frameHelper.frameCount, obj);
        }
    })

    frameHelper.render(scene, camera);

    if(recording && frameHelper.frameCount < animationFrames)
        frameHelper.saveFrame("bigbang", frameHelper.frameCount);
};


const setScale = (frameC, obj) => {
    const max = 3;
    const min = 0.2;

    const frame = frameC % animationFrames;
    if(frame < 10) {
        obj.scale.setLength(min);
    }
    else if(frame < 40) {
        const t = frameHelper.easeInOutQuint(THREE.Math.mapLinear(frame, 10, 40, 0, 1));
        const scale = THREE.Math.mapLinear(t, 0, 1, min, max);
        obj.scale.setLength(scale);
    }
    else if(frame < 100) {
        obj.scale.setLength(max);
    }
    else if(frame < 240) {
        const t = frameHelper.easeInOutQuad(THREE.Math.mapLinear(frame, 100, 240, 0, 1));
        const scale = THREE.Math.mapLinear(t, 0, 1, max, min);
        obj.scale.setLength(scale);
    }
}

const createLines = (cant, r, extension) => {
    let geometry = new THREE.BufferGeometry();

    let vertices = [...Array(cant)].reduce( (acc, cur, index) => {
        let vec = randomUnitVector();
        vec.multiplyScalar(r);

        let vec2 = vec.clone();
        vec2.multiplyScalar( Math.random() * 0.09 + extension );
        
        return [...acc, vec.x, vec.y, vec.z, vec2.x, vec2.y, vec2.z];
    }, [])

    geometry.addAttribute( 'position', new THREE.Float32BufferAttribute( vertices, 3 ) );
    
    return geometry;
}

const randomUnitVector = () => {
    let vec = new THREE.Vector3(Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1);
    vec.normalize();
    return vec;
}



// INIT 
init();