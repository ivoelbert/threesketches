var fs = require('file-system');

var frameRate = 60;
var frameCount = 0;
var mouseX = 0;
var mouseY = 0;


////////////////////////////////
// Handle mouse interaction
////////////////////////////////
document.addEventListener('mousemove', onDocumentMouseMove, false);
window.addEventListener('resize', onWindowResize, false);

function onDocumentMouseMove(event) {
    event.preventDefault();
    mouseX = (event.clientX / window.innerWidth) * 2 - 1;
    mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
}

function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
}
////////////////////////////////
// End handle mouse interaction
////////////////////////////////


////////////////////////////////
// Handle frame stuff
////////////////////////////////
function requestNextFrame( fun )
{
  setTimeout( function() {
      requestAnimationFrame( fun );
      frameCount++;
  }, 1000 / frameRate );
}

function setFrameRate( n )
{
  frameRate = n;
}
////////////////////////////////
// End handle frame stuff
////////////////////////////////


////////////////////////////////
// Handle save frame to jpg
////////////////////////////////
function saveFrame(name, f)
{
  let sufix = "";
  if(f !== undefined)
  {
    sufix = "0000" + f;
    sufix = sufix.slice(sufix.length - 4, sufix.length);
  }

  let dataUrl = renderer.domElement.toDataURL("image/jpeg");

  saveDataUrl(dataUrl, name + sufix);
}

function saveDataUrl(dataurl, name)
{
  try
  {
    // Decoding base-64 image
    // Source: http://stackoverflow.com/questions/20267939/nodejs-write-base64-image-file
    function decodeBase64Image(dataString, name)
    {
      var matches = dataString.match(/^data:([A-Za-z-+\/]+);base64,(.+)$/);
      var response = {};

      if (matches.length !== 3)
      {
        return new Error('Invalid input string');
      }

      response.type = matches[1];
      response.data = new Buffer(matches[2], 'base64');

      return response;
    }

    // Regular expression for image type:
    // This regular image extracts the "jpeg" from "image/jpeg"
    var imageTypeRegularExpression       = /\/(.*?)$/;

    var imageBuffer                      = decodeBase64Image(dataurl);
    var userUploadedFeedMessagesLocation = 'img/frames/';

    // This variable is actually an array which has 5 values,
    // The [1] value is the real image extension
    var imageTypeDetected                = imageBuffer
                                           .type
                                           .match(imageTypeRegularExpression);

    var userUploadedImagePath            = userUploadedFeedMessagesLocation +
                                           name +
                                           '.' +
                                           imageTypeDetected[1];

    // Guardo la imagen a disco
    fs.writeFile(userUploadedImagePath, imageBuffer.data, (err) => {
      if(err)
        console.log("ERROR! guardando " + userUploadedImagePath + "\n" + err);
    });
  }
  catch(error)
  {
    console.log('ERROR:', error);
  }
}
////////////////////////////////
// End handle save frame
////////////////////////////////

////////////////////////////////
// Handle object updates
////////////////////////////////

function handleUpdates(hObjectS)
{
  /*
    hObject es un JSON de la forma:
    "objName" : [{"type": "tipo", "param1": ..., ...}, ...]
  */
  let hObject = JSON.parse(hObjectS);
  let keys = Object.keys(hObject);

  for (let k = 0; k < keys.length; k++) {
    let key = keys[k];
    let objToUpdate = scene.getObjectByName(key);

    let updates = hObject[key];
    for(let i = 0; i < updates.length; i++)
    {
      let update = updates[i]
      switch(update.type)
      {
        case "setPosition":
        let pos = new THREE.Vector3(update.x, update.y, update.z);
        objToUpdate.position.copy(pos);
        break;

        case "translateOnAxis":
        let axis = new THREE.Vector3(update.x, update.y, update.z).normalize();
        let dist = update.dist;
        objToUpdate.translateOnAxis(axis, dist);
        break;

        case "applyQuaternion":
        break;

        case "rotateOnAxis":
        break;

        case "setUp":
        break;

        case "lookAt":
        break;

        case "setScale":
        break;

        case "applyMatrix":
        break;
      }
    }
  }

  return undefined;
}

////////////////////////////////
// End handle object updates
////////////////////////////////
