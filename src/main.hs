import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.Maybe
import Text.Printf

-- globals {{{
pixSize :: Int
pixSize = 10
pixs :: Int
pixs = 25

colors = [
         "#f8f8f8", "#b8b8b8", "#484848", "#000000",
         "#c11", "#f81", "#ee2", "#291",
         "#087", "#07f", "#14b", "#92b",
         "#941", "#fcb", "#f9b", "#a94"
         ]
-- }}}

-- draw pixel at (x,y).
-- also updates the inner data.
drawAt :: Env -> (Int,Int) -> IO ()
drawAt e loc@(x,y) = do
    brush <- readIORef (_brush e)
    writeArray (_data e) loc (_cid brush)
    render (_canv e) $
        translate (pixSize * fromIntegral x, pixSize * fromIntegral y)
        . color (readColor (_color  brush)) $ square

-- a square that corresponds to one pixel.
square :: Picture ()
square = fill $ rect (0,0) (fromIntegral pixSize, fromIntegral pixSize)

-- js string to Color. "rgb(r,g,b)" -> RGB r g b
readColor :: String -> Color
readColor ns = let (r,g,b) = read $ dropWhile (/= '(') ns in
    RGB r g b

setUp :: Env -> IO ()
setUp env = do
    let brush = _brush env
    -- setup onclick colorboxes
    colorboxes <- elemsByClass "colorbox"
    -- fill each box with its corresponding color.
    mapM_ (\(e,c) -> setStyle e "background-color" c) $ zip colorboxes colors
    -- when click on that element..
    mapM_ (\(e,i) -> (onEvent e OnClick (onBoxClick brush (e,i)))
          ) (zip colorboxes [0..])

onBoxClick :: IORef Brush -> (Elem, Int) -> Int -> (Int,Int) -> IO ()
onBoxClick brush (elm,n) = \ _ _ -> do
    -- clear previously selected box
    getBrushDOM brush >>=
        (\e -> setStyle e "border-color" "white") . fromJust
    newcolor <- getStyle elm "background-color"
    -- now we have the brush with new color.
    modifyIORef brush (setBrush newcolor n)
    readIORef brush >>= putStrLn . (++ " on " ++ (show n)) . show -- log
    -- highlight current color.
    getBrushDOM brush >>=
        (\e -> setStyle e "border-color" "black") . fromJust

toID :: Int -> ElemID
toID = ("color" ++ ) . printf "%02d"
getBrushDOM :: IORef Brush -> IO (Maybe Elem)
getBrushDOM b = readIORef b >>= elemById . toID . _cid

setBrush :: String -> Int -> Brush -> Brush
setBrush c n b = b { _color = c, _cid = n}

main = do
    Just canv <- getCanvasById "canv"
    b <- newIORef $ Brush (head colors) 0
    arr <- newArray ((0,0),(pixs,pixs)) 0 :: IO Pixels
    let env = Env b arr canv
    setUp env
    alert "Hi there!"
    render canv $ color (RGB 234 234 52) square

data Brush = Brush { _color :: String , _cid :: Int} deriving (Show)
type Pixels = IOArray (Int,Int) Int
-- environment.
-- _brush : Currently used brush,
-- _data : pixels
-- _canv : canvas to draw on
data Env = Env {_brush :: IORef Brush , _data :: Pixels, _canv :: Canvas}
