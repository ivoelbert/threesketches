var frameRate = 60;
var frameCount = 0;

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
