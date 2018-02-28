////////////////////////////////////////////////////////
// END PRINTED FROM HASKELL
////////////////////////////////////////////////////////

var animate = function () {

  requestNextFrame( animate );
  handleUpdates(Haste.updates(frameCount));
	renderer.render(scene, camera);

};

setFrameRate(30);
animate();
