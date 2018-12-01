const fs = require('file-system');
const THREE = require('three');
const electron = require('electron');

var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: false, antialias: true, alpha: true } );
renderer.setClearColor(0x000000, 1);
renderer.setSize( window.innerWidth, window.innerHeight );

document.body.appendChild( renderer.domElement );

module.exports.frameRate = 30;
module.exports.frameCount = 0;

////////////////////////////////
// Handle render
////////////////////////////////

const recording = b => {
    renderer.preserveDrawingBuffer = b;
}

const render = (scene, camera) => {
    renderer.render(scene, camera);
}

module.exports.recording = recording;
module.exports.render = render;
////////////////////////////////
// End render
////////////////////////////////


////////////////////////////////
// Handle frame stuff
////////////////////////////////

const resize = (w, h) => {
    electron.ipcRenderer.send('resize', w, h);
    renderer.setSize( w, h );
}

module.exports.resize = resize;
////////////////////////////////
// End handle frame stuff
////////////////////////////////


////////////////////////////////
// Handle frame stuff
////////////////////////////////
const requestNextFrame = fun => {
    setTimeout( () => {
        requestAnimationFrame( fun );
        module.exports.frameCount++;
    }, 1000 / module.exports.frameRate );
}

const setFrameRate = n => {
    module.exports.frameRate = n;
}

module.exports.requestNextFrame = requestNextFrame;
module.exports.setFrameRate = setFrameRate;
////////////////////////////////
// End handle frame stuff
////////////////////////////////


////////////////////////////////
// Handle save frame to jpg
////////////////////////////////
const saveFrame = (name, f) => {
    let sufix = "";
    if(f !== undefined)
    {
        sufix = "0000" + f;
        sufix = sufix.slice(sufix.length - 4, sufix.length);
    }

    const dataUrl = renderer.domElement.toDataURL("image/jpeg");

    saveDataUrl(dataUrl, name + sufix);
}

const saveDataUrl = (dataurl, name) => {
    try {
        // Decoding base-64 image
        // Source: http://stackoverflow.com/questions/20267939/nodejs-write-base64-image-file
        const decodeBase64Image = dataString => {
            const matches = dataString.match(/^data:([A-Za-z-+\/]+);base64,(.+)$/);
            let response = {};

            if (matches.length !== 3) {
                return new Error('Invalid input string');
            }

            response.type = matches[1];
            response.data = Buffer.from(matches[2], 'base64');

            return response;
        }

        // Regular expression for image type:
        // This regular image extracts the "jpeg" from "image/jpeg"
        const imageTypeRegularExpression = /\/(.*?)$/;

        const imageBuffer = decodeBase64Image(dataurl);

        // This variable is actually an array which has 5 values,
        // The [1] value is the real image extension
        const imageTypeDetected = imageBuffer.type.match(imageTypeRegularExpression)[1];

        const userUploadedImagePath = `img/frames/${name}.${imageTypeDetected}`;

        // Guardo la imagen a disco
        fs.writeFile(userUploadedImagePath, imageBuffer.data, err => {
            if(err)
                console.log("ERROR! guardando " + userUploadedImagePath + "\n" + err);
        });
    } catch(error) {
        console.log('ERROR:', error);
    }
}

module.exports.saveFrame = saveFrame;
////////////////////////////////
// End handle save frame
////////////////////////////////

