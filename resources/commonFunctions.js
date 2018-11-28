const randomUnitVector = () => {
    let vec = new THREE.Vector3(Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1);
    vec.normalize();
    return vec;
}

const randomVector = () => {
    let vec = new THREE.Vector3(Math.random() * 2 - 1, Math.random() * 2 - 1, Math.random() * 2 - 1);
    return vec;
}

const tabulate = (n, f) => [...new Array(n)].map( (_, i) => f(i) );

const repeat = (n, f) => {
    for(let i = 0; i < n; i++) {
        f(i);
    }
}

const easeInOutQuad = t => { return t<.5 ? 2*t*t : -1+(4-2*t)*t };
const easeInOutCubic = t => { return t<.5 ? 4*t*t*t : (t-1)*(2*t-2)*(2*t-2)+1 };
const easeInOutQuint = t => { return t<.5 ? 16*t*t*t*t*t : 1+16*(--t)*t*t*t*t };

module.exports.randomUnitVector = randomUnitVector;
module.exports.randomVector = randomVector;
module.exports.tabulate = tabulate;
module.exports.repeat = repeat;
module.exports.easeInOutQuad = easeInOutQuad;
module.exports.easeInOutCubic = easeInOutCubic;
module.exports.easeInOutQuint = easeInOutQuint;
