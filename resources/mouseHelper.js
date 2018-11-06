
module.exports.mouseX = 0;
module.exports.mouseY = 0;

////////////////////////////////
// Handle mouse interaction
////////////////////////////////

module.exports.mouseMove = event => {
    event.preventDefault();
    module.exports.mouseX = (event.clientX / window.innerWidth) * 2 - 1;
    module.exports.mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
}

document.addEventListener('mousemove', module.exports.mouseMove, false);

////////////////////////////////
// End handle mouse interaction
////////////////////////////////
