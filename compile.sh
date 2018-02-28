echo "Compiling..."
cp $1 src/resources/hs/scene.hs
cp $2 src/resources/hs/update.hs
cd src/resources/hs
ghc scene.hs
hastec '--start=$HASTE_MAIN();' update.hs
echo "ENDED - Please check for errors above."
