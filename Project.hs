-- Mustafa Sağlam - 150140129
module Project
  where
import Helper
import Prelude hiding (div)

-- rgb to hsv => (25, 50, 250) -> (233, 0.9, 0.98)
rgb2hsv :: RGB -> HSV
rgb2hsv (r,g,b) 
    | (r < 0 || r > 255) || (g < 0 || g > 255) || (b < 0 || b > 255) = error "Input is NOT an RGB color.\n\
        \Acceptable input is a tuple with intervals ([0 .. 255], [0 .. 255], [0 .. 255])"
    | otherwise = (calcH (max3 r g b) (min3 r g b), calcS (max3 r g b) (min3 r g b), calcV (max3 r g b))
      where 
        calcH :: Int -> Int -> Int
        calcH max min
            | max == min = 0
            | max == r   = round $ absAngle $ mul 60 $ fmod (div (g-b) (max-min)) 6
            | max == g   = round $ absAngle $ mul 60 $ add 2 $ div (b-r) (max-min)
            | max == b   = round $ absAngle $ mul 60 $ add 4 $ div (r-g) (max-min)
      
        calcS :: Int -> Int -> Float
        calcS max min
            | max == 0  = 0
            | otherwise = div (sub max min) max
   
        calcV :: Int -> Float
        calcV max = div max 255

-- hsv to rgb => (233, 0.9, 0.98) -> (25, 50, 250)
hsv2rgb :: HSV -> RGB
hsv2rgb (h,s,v)
    | (h < 0 || h > 360) || (s < 0.0 || s > 1.0) || (v < 0.0 || v > 1.0) = error "Input is NOT an HSV color.\n\
        \Acceptable input is a tuple with intervals ([0 .. 360], [0.0 .. 1.0], [0.0 .. 1.0])"
    | otherwise = toRGB (mul v s) (mul (sub 1 $ abs $ sub (fmod (div h 60) 2) 1) $ mul v s) (sub v $ mul v s)
    -- toRGB c x m => c = v*s, x = (1 - |(h/60) % 2 - 1|) * v*s, m = v - v*s
      where
        toRGB :: Float -> Float -> Float -> RGB
        toRGB c x m 
            |   0 <= h && h < 60  = (round $ mul 255 $ add c m, round $ mul 255 $ add x m, round $ mul 255 m)
            |  60 <= h && h < 120 = (round $ mul 255 $ add x m, round $ mul 255 $ add c m, round $ mul 255 m)
            | 120 <= h && h < 180 = (round $ mul 255 m,         round $ mul 255 $ add c m, round $ mul 255 $ add x m)
            | 180 <= h && h < 240 = (round $ mul 255 m,         round $ mul 255 $ add x m, round $ mul 255 $ add c m)
            | 240 <= h && h < 300 = (round $ mul 255 $ add x m, round $ mul 255 m,         round $ mul 255 $ add c m)
            | 300 <= h && h <=360 = (round $ mul 255 $ add c m, round $ mul 255 m,         round $ mul 255 $ add x m)

--given name find rgb
name2rgb :: String -> RGB
name2rgb name = findRGB name $ map splitOnColon $ lines rgbColors
                  where
                    findRGB :: String -> [[String]] -> RGB
                    findRGB n (c:cs)
                        | n == ""     = error "Color name is not acceptable"
                        | n == head c = read (c !! 1)::RGB
                        | cs == []    = error "Please make sure that the name of the color is correct.\n\
                            \Use lowercase color names such as 'maroon' or 'light blue' and not 'Maroon'.\n\
                            \140 lowercase HTML color names with spaces (more than one word) are usable as input."
                        | otherwise   = findRGB n cs

-- hsv to hsv, generate a list of hsvs                    
hsvGradient :: HSV -> HSV -> Int -> [HSV]
hsvGradient (h1,s1,v1) (h2,s2,v2) n 
    | n <  0 = []
    | n >= 0 = (h1,s1,v1):(hsvGradient (calcInter (sub h2 h1) (sub s2 s1) (sub v2 v1)) (h2,s2,v2) (n-1))
      where
        calcInter :: Float -> Float -> Float -> HSV
        calcInter dH dS dV = (round $ absAngle $ add h1 $ absAngle $ div dH n, add s1 $ div dS n, add v1 $ div dV n)
                          -- use of abs instead of absAngle ^here^ may result in reverse gradient

--name to name generate a list of hsvs
nameGradient :: String -> String -> Int -> [HSV]
nameGradient c1 c2 n = hsvGradient (rgb2hsv $ name2rgb c1) (rgb2hsv $ name2rgb c2) n

--description of colors, somewhat
hsv2desc :: HSV -> String
hsv2desc (h,s,v) = (hueName h) ++ ", " ++ (satName s) ++ " and " ++ (valName v)
  where
    hueName:: Int -> String
    hueName h
        | h <  15 = "red"
        | h <  20 = "reddish"
        | h <  45 = "orange"
        | h <  70 = "yellow"
        | h <  79 = "lime"
        | h < 163 = "green"
        | h < 193 = "cyan"
        | h < 240 = "blue"
        | h < 260 = "indigo"
        | h < 270 = "violet"
        | h < 291 = "purple"
        | h < 327 = "magenta"
        | h < 344 = "rose"
        | h < 360 = "red"
        | h < 0 || h > 360 = error "Incorrect Hue"
    
    satName:: Float -> String
    satName s
        | s < 0.04 = "grey"
        | s < 0.10 = "almost grey"
        | s < 0.30 = "very unsaturated"
        | s < 0.46 = "unsaturated"
        | s < 0.60 = "rather unsaturated"
        | s < 0.80 = "saturated"
        | s < 0.90 = "rather saturated"
        | s < 1.00 = "very saturated"
        | s < 0 || s > 1 = error "Incorrect Saturation"

    valName:: Float -> String
    valName v
        | v < 0.09 = "almost black"
        | v < 0.22 = "very dark"
        | v < 0.30 = "dark"
        | v < 0.60 = "normal"
        | v < 0.80 = "light"
        | v < 0.94 = "very light"
        | v < 1.00 = "almost white"
        | v < 0 || v > 1 = error "Incorrect Value"