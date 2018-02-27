////////////////////////////////////////////////////////
// END PRINTED FROM HASKELL
////////////////////////////////////////////////////////

var animate = function () {

  requestNextFrame( animate );
  console.log(Haste.updates(frameCount));
	renderer.render(scene, camera);

};

setFrameRate(30);
animate();
