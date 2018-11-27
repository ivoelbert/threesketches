const randomUnitVector = () => {
    let vec = new THREE.Vector3(Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1);
    vec.normalize();
    return vec;
}

const randomVector = () => {
    let vec = new THREE.Vector3(Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1);
    return vec;
}

module.exports.randomUnitVector = randomUnitVector;
module.exports.randomVector = randomVector;

////////////////////////////////
// Handle easing functions
////////////////////////////////

module.exports.easeInOutQuad = t => { return t<.5 ? 2*t*t : -1+(4-2*t)*t };
module.exports.easeInOutCubic = t => { return t<.5 ? 4*t*t*t : (t-1)*(2*t-2)*(2*t-2)+1 };
module.exports.easeInOutQuint = t => { return t<.5 ? 16*t*t*t*t*t : 1+16*(--t)*t*t*t*t };

////////////////////////////////
// End handle easing functions
////////////////////////////////
