var frameRate = 60;
var frameCount = 0;
var mouseX = 0;
var mouseY = 0;



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
