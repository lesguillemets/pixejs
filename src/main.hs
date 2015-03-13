import Haste
import Haste.DOM
import Haste.Graphics.Canvas
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.Char
import Data.Maybe
import Control.Monad
import Numeric
import Text.Printf

-- globals {{{
pixSize :: Int
pixSize = 10
pixs :: Int
pixs = 18

baseURL = "http://lesguillemets.github.io/pixejs/?"
version = "0"

colors = [
         "#f8f8f8", "#b8b8b8", "#484848", "#000000",
         "#cc1111", "#ff8811", "#eeee22", "#229911",
         "#008877", "#0077ff", "#1144bb", "#9922bb",
         "#994411", "#ffccbb", "#ff99bb", "#aa9944"
         ]
-- }}}
hexToRGB :: String -> Color
hexToRGB hexcode = let hx = tail hexcode
                       (r,hx') = splitAt 2 hx
                       (g,b) = splitAt 2 hx'
                       f = fst . head . readHex
                       in
                           RGB (f r) (f g) (f b)

-- draw pixel at (x,y).
-- also updates the inner data.
drawAt :: Env -> (Int,Int) -> IO ()
drawAt e loc@(x,y) = do
    brush <- readIORef (_brush e)
    writeArray (_data e) loc (_cid brush)
    renderOnTop (_canv e) $
        translate (fromIntegral (pixSize * x), fromIntegral (pixSize * y))
        . color (readColor (_color  brush)) $ square

putColor :: Canvas -> (Int,Int) -> Color -> IO()
putColor c (x,y) col = renderOnTop c $
    translate (fromIntegral (pixSize * x), fromIntegral (pixSize * y))
    . color col $ square


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
    
    -- setup export button
    Just ex <- elemById "export"
    onEvent ex OnClick (\_ _ -> mkURL env)
    -- setup import (for debugging)
    Just imp <- elemById "import"
    onEvent imp OnClick (\_ _ -> do
                        Just txtb <- elemById "dat"
                        getProp txtb "innerHTML" >>= readData env
                        )
    
    -- setup canvas
    onEvent (canvasElem (_canv env)) OnClick (onCanvClick env)
    putStrLn "ready."

onBoxClick :: IORef Brush -> (Elem, Int) -> Int -> (Int,Int) -> IO ()
onBoxClick brush (elm,n) = \ _ _ -> do
    -- clear previously selected box
    getBrushDOM brush >>=
        (\e -> setStyle e "border-color" "white") . fromJust
    newcolor <- getStyle elm "background-color"
    -- now we have the brush with new color.
    modifyIORef brush (setBrush newcolor n)
    -- readIORef brush >>= putStrLn . (++ " on " ++ (show n)) . show
    -- highlight current color.
    getBrushDOM brush >>=
        (\e -> setStyle e "border-color" "black") . fromJust

onCanvClick :: Env -> Int -> (Int,Int) -> IO ()
onCanvClick e = \ _ (x,y) -> let pos = (x `div` pixSize, y `div` pixSize)
        in
            drawAt e pos

toID :: Int -> ElemID
toID = ("color" ++ ) . printf "%02d"
getBrushDOM :: IORef Brush -> IO (Maybe Elem)
getBrushDOM b = readIORef b >>= elemById . toID . _cid

setBrush :: String -> Int -> Brush -> Brush
setBrush c n b = b { _color = c, _cid = n}

main = do
    Just canv <- getCanvasById "canv"
    b <- newIORef $ Brush (head colors) 0
    arr <- newArray ((0,0),(pixs-1,pixs-1)) 0 :: IO Pixels
    let env = Env b arr canv
    setUp env

data Brush = Brush { _color :: String , _cid :: Int} deriving (Show)
type Pixels = IOArray (Int,Int) Int
-- environment.
-- _brush : Currently used brush,
-- _data : pixels
-- _canv : canvas to draw on
data Env = Env {_brush :: IORef Brush , _data :: Pixels, _canv :: Canvas}

-----

mkURL :: Env -> IO ()
mkURL e = do
    s <- toDataString (_data e)
    Just txtArea <- elemById "dat"
    setProp txtArea "innerHTML" s

toFullURL :: String -> String
toFullURL dat = baseURL ++ "d=" ++ dat ++ "&v=" ++ version

readData :: Env -> String -> IO ()
readData e dat = do
    np <- fromDataString dat
    cpPixels np (_data e)
    resetCanv e

resetCanv :: Env -> IO ()
resetCanv e = forM_ [0..pixs-1] $ \x ->
                    forM_ [0..pixs-1] $ \y -> do
                        col <- readArray (_data e) (x,y)
                        putColor (_canv e) (x,y) (hexToRGB $ colors !! col)

toDataString :: Pixels -> IO String
toDataString p = liftM encode $ getElems p
fromDataString :: String -> IO Pixels
fromDataString c = newListArray ((0,0),(pixs-1,pixs-1)) (decode c)

encode :: [Int] -> String
encode = map (chr . (+) 97)
decode :: String -> [Int]
decode = map (subtract 97 . ord)

cpPixels :: Pixels -> Pixels -> IO ()
-- cp from to
cpPixels p0 p1 = do
    ((x0,y0), (x1,y1)) <- getBounds p0
    forM_ [x0..x1] $ \x ->
        forM_ [y0..y1] $ \y ->
            readArray p0 (x,y) >>= writeArray p1 (x,y)
-- FIXME :: Too far a workaround
