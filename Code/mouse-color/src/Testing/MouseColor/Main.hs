module Main where

import Control.Monad.IO.Class

import Data.IORef
import Data.Time.Clock.POSIX

import FRP.TimeFlies.SignalFunctions

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


-- Skeleton from http://www.haskell.org/haskellwiki/OpenGLTutorial1
myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = map (\k -> (sin (2 * pi * k / 12) ,cos ( 2 * pi * k /12) ,0.0 ) ) [1..12]

main :: IO ()
main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayRef <- newIORef $ return ()

  time <- fmap realToFrac getPOSIXTime
  
  let mouseApplicationState = initSFEval (sdHS (\act -> writeIORef displayRef act >> postRedisplay Nothing)) time mouseApplicationSF
  sfRef <- newIORef mouseApplicationState
  
  keyboardMouseCallback $= Just (\key keystate _ pos -> do sfSt <- readIORef sfRef
                                                           ((), sfSt') <- runSFEvalT (do case keystate of
                                                                                           Down -> push $ eoRight $ eo key
                                                                                           Up -> return ()
                                                                                         update (sdLeft $ sd pos))
                                                                                     sfSt
                                                           writeIORef sfRef sfSt')

  passiveMotionCallback $= Just (\pos -> do sfSt <- readIORef sfRef
                                            ((), sfSt') <- runSFEvalT (update (sdLeft $ sd pos))
                                                                      sfSt
                                            writeIORef sfRef sfSt')  



  initialDisplayMode $= [DoubleBuffered]
  displayCallback $= do clear [ColorBuffer]
                        act <- readIORef displayRef
                        act
                        swapBuffers
                        flush

  addTimerCallback 33 $ let tc = do addTimerCallback 33 tc
                                    sfSt <- readIORef sfRef
                                    ((), sfSt') <- runSFEvalT (do time <- fmap realToFrac $ liftIO getPOSIXTime
                                                                  sample time)
                                                   sfSt
                                    writeIORef sfRef sfSt'
                        in tc

  mainLoop


mouseApplicationSF :: SF NonInitialized (SVAppend (SVSignal Position) (SVEvent Key)) (SVSignal (IO ()))
mouseApplicationSF = second ignore >>> cancelRight >>> pureSignalTransformer (\(Position x y) -> let r = fromIntegral (x `mod` 50) / 50 :: GLdouble
                                                                                                     g = fromIntegral (y `mod` 50) / 50 :: GLdouble
                                                                                                     b = 0.5 :: GLdouble
                                                                                                 in renderPrimitive Quads $ color (Color4 r g b 1.0) >>
                                                                                                                            mapM_ vertex ([Vertex3 (-1) 1 0,
                                                                                                                                           Vertex3 1 1 0,
                                                                                                                                           Vertex3 1 (-1) 0,
                                                                                                                                           Vertex3 (-1) (-1) 0] :: [Vertex3 GLdouble]))