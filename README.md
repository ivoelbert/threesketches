# Node.js + three.js + Haskell sketches

Write (very basic) [three.js](https://threejs.org/) sketches in Haskell

## Getting Started

This will **NOT** be easy to set up. You will need GHC, [Node.js](https://nodejs.org/), [Haste](https://haste-lang.org/), and god knows what else.

### Installing

Clone this repository and go to *src/* directory. Once there you will need to run

```
npm install
```

Already there will be some test files so you can run

```
npm start
```

And see if everything is working. If not, hopefully you will be notified which npm packages you need to install. For that just run

```
npm install -g -package-
```

Once you are able to run the test sketch (just a white ball rolling around on a green surface) you can try compiling some sketches. Go back to the main directory and try running

```
./compile.sh sketches/sceneTest.hs sketches/updateTest.hs
```

At this point Haste should complain about who knows what. If any package is needed try

```
haste-cabal install -package-
```

Whenever you can compile successfully run

```
./setup.sh
```

and to see the sketch

```
./run.sh
```

Congratulations! You can now write simple sketches in Haskell as will be explained.

## Writing your own sketches.

A sketch is divided in two .hs files: a *scene* and an *update*

The *scene* is what statically describes what will be seen. In a *scene* you will have some 3D objects, some lights to shine on them and a camera to render your scene to the screen.

The *update* describes how objects in your *scene* will behave in 3D space over time. Here you will be able to transform your objects, lights and camera. You can move them around, rotate them, etc.

### Describing your scene

First let's describe *myFirstScene.hs*. You will need to import one module

```haskell
import ThreeScene
```

Then you can create your scene which must be of type

```haskell
myFirstScene :: ThreeScene ()
```

Set up a camera, an ambient light and create a cube

```haskell
myFirstScene = do newScene
                  cam <- perspectiveCamera 75 0.1 100
                  addToScene "camera" cam
                  light <- ambientLight (hex "fff7d1") 1
                  addToScene "light" light
                  cubeGeom <- boxGeometry 1 1 1
                  cubeMat <- standardMaterial (hex "e56244") (hex "000000") 1 0.4
                  cubeMesh <- createMesh cubeGeom cubeMat
                  addToScene "cube" cubeMesh
```

and write your main function which must export your scene to *src/resources/js/SKETCH.js* as follows

```haskell
main :: IO ()
main = do printScene myFirstScene
```

That's it! Your first scene consists on a red-ish cube illuminated by a slightly warm ambient light.

### Describing your update

Let's now describe *myFirstUpdate.hs*. As before, there is a little prologue to follow

```haskell
{-# LANGUAGE OverloadedStrings #-}
import ThreeUpdate
```

Your animation will be a function of type

```haskell
myFirstUpdate :: Double -> ThreeAnimation ()
```

this function describes your objects transformations frame-by-frame. That is, this function will be called each frame and passed the frame number as an argument. Sketches will be run at 30fps.

You can now set the position of the camera and rotate the cube by the Y axis as follows

```haskell
myFirstUpdate frame = do
                      newAnimation
                      setPosition "camera" (vector 0 2 3)
                      setUp "camera" (vector 0 1 0)
                      lookAt "camera" (vector 0 0 0)
                      rotateOnAxis "cube" (vector 0 1 0) 0.05
```

Finally write your main function which will export your update function

```haskell
main = do
       export "updates" $ createUpdateFunction myFirstUpdate
```

Great! Now you can compile and run your sketch.

More on what you can do in your *scene* and *update* in [DOCUMENTATION.md](https://github.com/ivoelbert/threesketches/blob/master/DOCUMENTATION.md)

If things go south you can always base your sketch on *sketches/sceneTest.hs* and *sketches/updateTest.hs*

## Compiling and running your sketch

In the main directory you will find three scripts: *compile.sh* *setup.sh* and *run.sh* (if necessary make them executable).

To compile your previously made sketch run

```
./compile.sh sketches/myFirstScene.sh sketches/myFirstUpdate.sh
```

Check for errors, and if everything is okay run

```
./setup.sh
```

This will set your sketch up for running. Check for errors, if your scene has no camera, for example, error will pop up at this time.

To run your sketch (after compiling and setting up successfully) run

```
./run.sh
```

You should now see your sketch running in a 800 * 600 window with a javascript console for debugging.

If you want to manually (Javascript) add things to your sketch, src/resources/js/SKETCH.js is the file exported by your Haskell scene.
