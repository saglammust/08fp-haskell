-- Mustafa Sağlam - 150140129
module Helper
  where
import Prelude hiding (div)

type RGB = (Int, Int, Int)     -- 255, 255, 255
type HSV = (Int, Float, Float) -- 240, 0.2, 0.8

---truple index functions
first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z

--unannoying versions of basic arithmetic operations
div :: (Real a, Real b) => a -> b -> Float
div x y = (realToFrac x) / (realToFrac y)

mul :: (Real a, Real b) => a -> b -> Float
mul x y = (realToFrac x) * (realToFrac y)

add :: (Real a, Real b) => a -> b -> Float
add x y = (realToFrac x) + (realToFrac y)

sub :: (Real a, Real b) => a -> b -> Float
sub x y = (realToFrac x) - (realToFrac y)

fmod :: (Real a, Num a) => a -> a -> Float
fmod x y = sub x $ mul y $ truncate $ div x y

--maximum and minimum of 3 numerics
max3 :: (Num a, Ord a) => a -> a -> a -> a
max3 x y z
    | x > y && x > z = x
    | y > z          = y
    | otherwise      = z

min3 :: (Num a, Ord a) => a -> a -> a -> a
min3 x y z
    | x < y && x < z = x
    | y < z          = y
    | otherwise      = z

-- positive angle value for hue
absAngle :: Float -> Float
absAngle r 
    | r <  0     = absAngle $ add r 360
    | r >= 360   = absAngle $ sub r 360
    | otherwise = r 

-- string spliter function, inspired from Prelude.words
splitWith :: (Char -> Bool) -> String -> [String]
splitWith df s =  case dropWhile df s of
                      "" -> []
                      s' -> w : splitWith df s''
                            where (w, s'') = break df s'

splitOnColon = splitWith (==':') -- currying for practical use

rgbColors :: String
rgbColors = "maroon:(128,0,0)\n\
\dark red:(139,0,0)\n\
\brown:(165,42,42)\n\
\firebrick:(178,34,34)\n\
\crimson:(220,20,60)\n\
\red:(255,0,0)\n\
\tomato:(255,99,71)\n\
\coral:(255,127,80)\n\
\indian red:(205,92,92)\n\
\light coral:(240,128,128)\n\
\dark salmon:(233,150,122)\n\
\salmon:(250,128,114)\n\
\light salmon:(255,160,122)\n\
\orange red:(255,69,0)\n\
\dark orange:(255,140,0)\n\
\orange:(255,165,0)\n\
\gold:(255,215,0)\n\
\dark golden rod:(184,134,11)\n\
\golden rod:(218,165,32)\n\
\pale golden rod:(238,232,170)\n\
\dark khaki:(189,183,107)\n\
\khaki:(240,230,140)\n\
\olive:(128,128,0)\n\
\yellow:(255,255,0)\n\
\yellow green:(154,205,50)\n\
\dark olive green:(85,107,47)\n\
\olive drab:(107,142,35)\n\
\lawn green:(124,252,0)\n\
\chart reuse:(127,255,0)\n\
\green yellow:(173,255,47)\n\
\dark green:(0,100,0)\n\
\green:(0,128,0)\n\
\forest green:(34,139,34)\n\
\lime:(0,255,0)\n\
\lime green:(50,205,50)\n\
\light green:(144,238,144)\n\
\pale green:(152,251,152)\n\
\dark sea green:(143,188,143)\n\
\medium spring green:(0,250,154)\n\
\spring green:(0,255,127)\n\
\sea green:(46,139,87)\n\
\medium aqua marine:(102,205,170)\n\
\medium sea green:(60,179,113)\n\
\light sea green:(32,178,170)\n\
\dark slate gray:(47,79,79)\n\
\teal:(0,128,128)\n\
\dark cyan:(0,139,139)\n\
\aqua:(0,255,255)\n\
\cyan:(0,255,255)\n\
\light cyan:(224,255,255)\n\
\dark turquoise:(0,206,209)\n\
\turquoise:(64,224,208)\n\
\medium turquoise:(72,209,204)\n\
\pale turquoise:(175,238,238)\n\
\aqua marine:(127,255,212)\n\
\powder blue:(176,224,230)\n\
\cadet blue:(95,158,160)\n\
\steel blue:(70,130,180)\n\
\corn flower blue:(100,149,237)\n\
\deep sky blue:(0,191,255)\n\
\dodger blue:(30,144,255)\n\
\light blue:(173,216,230)\n\
\sky blue:(135,206,235)\n\
\light sky blue:(135,206,250)\n\
\midnight blue:(25,25,112)\n\
\navy:(0,0,128)\n\
\dark blue:(0,0,139)\n\
\medium blue:(0,0,205)\n\
\blue:(0,0,255)\n\
\royal blue:(65,105,225)\n\
\blue violet:(138,43,226)\n\
\indigo:(75,0,130)\n\
\dark slate blue:(72,61,139)\n\
\slate blue:(106,90,205)\n\
\medium slate blue:(123,104,238)\n\
\medium purple:(147,112,219)\n\
\dark magenta:(139,0,139)\n\
\dark violet:(148,0,211)\n\
\dark orchid:(153,50,204)\n\
\medium orchid:(186,85,211)\n\
\purple:(128,0,128)\n\
\thistle:(216,191,216)\n\
\plum:(221,160,221)\n\
\violet:(238,130,238)\n\
\magenta:(255,0,255)\n\
\fuchsia:(255,0,255)\n\
\orchid:(218,112,214)\n\
\medium violet red:(199,21,133)\n\
\pale violet red:(219,112,147)\n\
\deep pink:(255,20,147)\n\
\hot pink:(255,105,180)\n\
\light pink:(255,182,193)\n\
\pink:(255,192,203)\n\
\antique white:(250,235,215)\n\
\beige:(245,245,220)\n\
\bisque:(255,228,196)\n\
\blanched almond:(255,235,205)\n\
\wheat:(245,222,179)\n\
\corn silk:(255,248,220)\n\
\lemon chiffon:(255,250,205)\n\
\light golden rod yellow:(250,250,210)\n\
\light yellow:(255,255,224)\n\
\saddle brown:(139,69,19)\n\
\sienna:(160,82,45)\n\
\chocolate:(210,105,30)\n\
\peru:(205,133,63)\n\
\sandy brown:(244,164,96)\n\
\burly wood:(222,184,135)\n\
\tan:(210,180,140)\n\
\rosy brown:(188,143,143)\n\
\moccasin:(255,228,181)\n\
\navajo white:(255,222,173)\n\
\peach puff:(255,218,185)\n\
\misty rose:(255,228,225)\n\
\lavender blush:(255,240,245)\n\
\linen:(250,240,230)\n\
\old lace:(253,245,230)\n\
\papaya whip:(255,239,213)\n\
\sea shell:(255,245,238)\n\
\mint cream:(245,255,250)\n\
\slate gray:(112,128,144)\n\
\light slate gray:(119,136,153)\n\
\light steel blue:(176,196,222)\n\
\lavender:(230,230,250)\n\
\floral white:(255,250,240)\n\
\alice blue:(240,248,255)\n\
\ghost white:(248,248,255)\n\
\honeydew:(240,255,240)\n\
\ivory:(255,255,240)\n\
\azure:(240,255,255)\n\
\snow:(255,250,250)\n\
\black:(0,0,0)\n\
\dim gray:(105,105,105)\n\
\gray:(128,128,128)\n\
\dark gray:(169,169,169)\n\
\silver:(192,192,192)\n\
\light gray:(211,211,211)\n\
\gainsboro:(220,220,220)\n\
\white smoke:(245,245,245)\n\
\white:(255,255,255)"