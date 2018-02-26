let esferaGeometry = new THREE.SphereBufferGeometry( 1.0, 32, 32 );
let esferaMaterial = new THREE.MeshStandardMaterial( {
    color: new THREE.Color(0.8, 0.2, 0.0),
    emissive: new THREE.Color(0.0, 0.0, 0.0),
    roughness: 0.5,
    metalness: 1.0
} );
let esferaMesh = new THREE.Mesh( esferaGeometry, esferaMaterial );
scene.add( esferaMesh );

let cuboGeometry = new THREE.BoxBufferGeometry( 1.5, 3.0, 1.5 );
let cuboMaterial = new THREE.MeshStandardMaterial( {
    color: 0xaaaaaa,
    emissive: 0x000000,
    roughness: 1.0,
    metalness: 0.0
} );
let cuboMesh = new THREE.Mesh( cuboGeometry, cuboMaterial );
scene.add( cuboMesh );

