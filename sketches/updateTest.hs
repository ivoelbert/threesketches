{-# LANGUAGE OverloadedStrings #-}

import ThreeUpdate

update :: Double -> ThreeAnimation ()
update frame = do
               newAnimation
               setPosition "dirLight" (vector (-3) 3 5)
               setPosition "camara" (vector 0 3 5)
               setUp "camara" (vector 0 1 0)
               lookAt "camara" (vector 0 0 0)
               setPosition "mesa" (vector 0 (-0.5) 0)
               setPosition "bola" (vector (cos (frame * 0.05)) 0.5 (sin (frame * 0.05)))

main = do
       export "updates" $ createUpdateFunction update
