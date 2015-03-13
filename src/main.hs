import Haste
import Haste.DOM
import Haste.Graphics.Canvas

colors = [
         "#000000", "#484848", "#b8b8b8", "f8f8f8",
         "#c11", "#f81", "#ee2", "#291",
         "#087", "#07f", "#14b", "#92b",
         "#941", "#fcb", "#f9b", "#a94"
         ]

square' :: Shape ()
square' = do
    rect (10,10) (20,20)

setUp :: IO ()
setUp = do
    colorboxes <- elemsByClass "colorbox"
    mapM_ (\(e,c) -> setStyle e "background-color" c) $
        zip colorboxes colors
    mapM_ (\e -> (onEvent e OnClick (\_ _-> print "HI"))) colorboxes

main = do
    Just canv <- getCanvasById "canv"
    setUp
    alert "Hi there!"
    render canv $ do
        color (RGB 234 234 52) . fill $ square'
