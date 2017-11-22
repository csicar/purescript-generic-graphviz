module Graphics.ConsoleRender where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Semigroupoid ((>>>))
import Data.Eq ((/=))
import Data.EuclideanRing ((/))
import Data.Int (pow, round, toNumber)
import Data.Ord (abs, max)
import Data.Semigroup (class Semigroup, (<>))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Math (atan, pi, sqrt, tan)


data Point = Point Int Int

data Line = Line Point Point

data Rectangle = Rectangle Point Point

data Ellipse = Ellipse Point Int Int

data Text = Text Point String


height :: Line -> Int
height (Line (Point x1 y1) (Point x2 y2)) = y2 - y1

width :: Line -> Int
width (Line (Point x1 y1) (Point x2 y2)) = x2 - x1

absHeight :: Line -> Int
absHeight = height >>> abs

absWidth :: Line -> Int
absWidth = width >>> abs

angle :: Line -> Rotation
angle l = round $ (360.0 / (2.0*pi)) * atan (toNumber (height l ) / toNumber (width l))

type DrawResult =
  { endPoint :: Point
  , text :: String
  , css :: Array String
  }

length :: Line -> Number
length line = sqrt $ toNumber $ (pow (height line) 2) + (pow (width line) 2)

drawLine :: Int -> Line -> Point -> DrawResult
drawLine thickness line (Point x y)
  = {
      css: [css],
      endPoint : Point (x +(max x1 x2)) 0,
      text: "%c "
    }
  where
    percentageThickness = round $ 100.0*((toNumber thickness) / (length line))
    lower = show $ 50 - percentageThickness
    upper = show $ 50 + percentageThickness
    Line (Point x1 y1) (Point x2 y2) = line
    css
      = "height: " <> (show $ absHeight line + thickness) <> "px; "
      <> "width: " <> (show $ absWidth line + thickness) <> "px; "
      <> "font-size: 0; "
      <> "margin-left: " <> (show $ x1 - x) <> "px; "
      <> "margin-top: " <> (show $ y1 - y) <> "px; "
      <> "border-radius: " <> (show thickness) <> "px; "
      <> "background: linear-gradient(" <> (show $ angle line) <>  "deg, "
        <>"rgba(0,0,0,0) 0%,"
        <>"rgba(0,0,0,0) " <> lower <> "%,"
        <>"rgba(255,255,255,1) " <> lower <> "%,"
        <>"rgba(255,255,255,1) 50%,"
        <>"rgba(255,255,255,1) "<> upper <> "%,"
        <>"rgba(0,0,0,0) "<> upper <> "%,"
        <>"rgba(0,0,0,0) 100%); "


addPoint :: Point -> Point -> Point
addPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

type Rotation = Int

-- | Text to show, font size,, offset
drawText :: Text -> Int -> Point -> DrawResult
drawText (Text (Point x y) text) size (Point offX offY)
  = {
    css: [css],
    endPoint: Point x 0,
    text: "%c " <> text
  }
  where
    css
      = "font-size: " <> (show size) <> "px; "
      <> "width: 1px; "
      <> "white-space: nowrap; "
      <> "margin-left: " <> (show $ x - offX) <> "px; "
      <> "margin-top: " <> (show $ y - offY) <> "px; "

drawArrowHead :: Line -> Point -> DrawFunc
drawArrowHead dir point
  =  (DrawFunc $ drawLine 2 (Line point (point `addPoint` (Point 20 20))))
  <> (DrawFunc $ drawLine 2 (Line point (point `addPoint` (Point (-20) 20))))

drawRectangle :: Int -> Rectangle -> String -> Point -> DrawResult
drawRectangle roundness (Rectangle (Point x1 y1) (Point x2 y2)) text (Point offX offY)
  = {
    css: [css],
    endPoint: Point (max x1 x2) 0,
    text: "%c " <> text
  }
  where
    height' = (show $ abs $ y2 - y1)
    width' = (show $ abs $ x2 - x1)
    css
      = "height: " <> height' <> "px; "
      <> "width: " <> width' <> "px; "
      <> "margin-left: " <> (show $ x1 - offX) <> "px; "
      <> "margin-top: " <> (show  $ y1 - offY) <> "px; "
      <> "border-radius: " <> (show $ roundness) <> "%; "
      <> "text-align: center; "
      <> "line-height: "<> height' <>"px; "
      <> "border: 1px solid white; "



ellipseBoundingBox :: Ellipse -> Rectangle
ellipseBoundingBox (Ellipse (Point x y) rx ry) = Rectangle (Point (x-rx) (y-ry)) (Point (x+rx) (y+ry))

drawEllipse :: Ellipse -> String -> Point -> DrawResult
drawEllipse ellipse text offset = drawRectangle 100 (ellipseBoundingBox ellipse) text offset


drawPoint :: Point -> DrawFunc
drawPoint point = DrawFunc $ drawEllipse (Ellipse point 10 10) ""

newtype DrawFunc = DrawFunc (Point -> DrawResult)

instance drawFuncSemigroup :: Semigroup (DrawFunc) where
  append :: DrawFunc -> DrawFunc -> DrawFunc
  append (DrawFunc f) (DrawFunc g) = DrawFunc $ \point -> let
    {endPoint, text, css} = (f point)
    {endPoint: endPoint2, text: text2, css: css2} = (g endPoint)
    in
    {endPoint: endPoint2,text: text <> text2, css: css <> css2}

complete :: DrawFunc -> String
complete (DrawFunc f) =
  let
    {css, text}= f (Point 0 0)
  in
    "console.log("<> show text <> ",\n " <> (joinWith ",\n " $ show <$> css) <> ")"

logComplete :: DrawFunc
  -> Eff
       ( console :: CONSOLE
       )
       Unit
logComplete s = log $ complete s

ex2line = Line (Point 100 200) (Point 200 300)
o = drawPoint $ Point 0 0

ex1 = DrawFunc $ drawEllipse (Ellipse (Point 100 200) 100 200) "asdd"
ex2 = DrawFunc $ drawLine 5 $ ex2line
ex2' = DrawFunc $ drawLine 10 $ ex2line
ex3 = DrawFunc $ drawText (Text (Point 100 200) "Hi from text" ) 20
ex4 = drawArrowHead ex2line (Point 200 200)

exGraphviz :: DrawFunc
exGraphviz
  = p <> sb <> s <> b <> end
  where
    s = DrawFunc $ drawEllipse (Ellipse (Point 63 89) 27 17) "s"
    b = DrawFunc $ drawEllipse (Ellipse (Point 27 18) 27 18) "b"
    end = DrawFunc $ drawEllipse (Ellipse (Point 99 18) 27 18) "end"
    sb = DrawFunc $ drawLine 3 $ Line (Point 54 72) (Point 40 40)
    p = drawPoint $ Point 40 40
