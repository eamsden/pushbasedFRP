module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


-- Skeleton from http://www.haskell.org/haskellwiki/OpenGLTutorial1
myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = map (\k -> (sin (2 * pi * k / 12) ,cos ( 2 * pi * k /12) ,0.0 ) ) [1..12]

main :: IO ()
main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display
  mainLoop
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) myPoints
  flush