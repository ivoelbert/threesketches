echo "Setting up..."
cd src/resources/hs
./scene
cp update.js ../js/update.js
echo "Cleaning up..."
rm *.o
rm *.hi
rm *.jsmod
rm update.js
echo "ENDED - Please check for errors above."
