import Haste
import Haste.Graphics.Canvas

square' :: Shape ()
square' = do
    rect (10,10) (20,20)

main = do
    Just canv <- getCanvasById "canv"
    alert "Hi there!"
    render canv $ do
        color (RGB 234 234 52) . fill $ square'
