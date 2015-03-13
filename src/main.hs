import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO

colors = [
         "#000000", "#484848", "#b8b8b8", "f8f8f8",
         "#c11", "#f81", "#ee2", "#291",
         "#087", "#07f", "#14b", "#92b",
         "#941", "#fcb", "#f9b", "#a94"
         ]

square' :: Shape ()
square' = do
    rect (10,10) (20,20)

setUp :: Env -> IO ()
setUp env = do
    let brush = _brush env
    colorboxes <- elemsByClass "colorbox"
    mapM_ (\(e,c) -> setStyle e "background-color" c) $ zip colorboxes colors
    mapM_ (\(e,i) -> (onEvent e OnClick (onBoxClick brush (e,i)))
          ) (zip colorboxes [0..])

onBoxClick :: IORef Brush -> (Elem, Int) -> Int -> (Int,Int) -> IO ()
onBoxClick brush (elm,n) = \ _ _ -> do
    newcolor <- getStyle elm "background-color"
    modifyIORef brush (setBrush newcolor n)
    readIORef brush >>= putStrLn . (++ " on " ++ (show n)) . show

setBrush :: String -> Int -> Brush -> Brush
setBrush c n b = b { _color = c, _cid = n}

main = do
    Just canv <- getCanvasById "canv"
    b <- newIORef $ Brush (head colors) 0
    arr <- newArray (0,10) 15 :: IO (IOArray Int Int)
    let env = Env b arr
    setUp env
    alert "Hi there!"
    render canv $ do
        color (RGB 234 234 52) . fill $ square'

data Brush = Brush { _color :: String , _cid :: Int} deriving (Show)
type Pixels = IOArray Int Int
data Env = Env {_brush :: IORef Brush , _pixs :: Pixels}
