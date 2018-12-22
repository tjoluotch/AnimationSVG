module MyAnimation where

import Animation

-- remeber (0,0) otherwise known as the origin is always at the top left of the picture
picture :: Animation

picture =
    triLimeLTR
    `plus`
    triLimeRTL
    `plus`
    triTealLTR
    `plus`
    triTealRTL
    `plus`
    withPaint (cycleSmooth 2 [red,green, blue])
        (combine 
            [translate (always (a,200))
                (rotate (spinner 2) ctrlPiece) | a <- [350,340..100]])
    `plus`     
    withPaint (cycleSmooth 2 [red,green, blue])
        (combine 
            [translate (always (a,200))
                (rotate (spinner w) ctrlPiece) | a <- [380,390..600], w <- [-2]])
    `plus`
    translate (always (500,200))
        (rotate (always 90)
            (scale (cycleSmooth 4 [(1.5, 4), (4, 1.5)])
                (withPaint (cycleSmooth 2 [fuchsia, purple])
                    ctrlPiece)))
    `plus`
    translate (always (700,200))
        (rotate (always 90)
            (scale (cycleSmooth 4 [(1.5, 4), (4, 1.5)])
                (withPaint (cycleSmooth 2 [purple, fuchsia])
                    ctrlPiece)))            


 -- List Function that increments from 0 up to 800 in 50's               
listA :: [Length]
listA = [0,50..800]

-- Use listA values and wrap these in a tuple with 0 values
moveRight ::[(Length, Length)]
moveRight = [(hor,vert) | hor <- listA, vert <- [0]]

-- function of type Animation which is a right facing triangle
rightTri :: Animation
rightTri = polygon [(0,0), (75,50), (0,100)]

-- function of type Animation which is a left facing triangle
leftTri :: Animation
leftTri = polygon[(75,0), (0,50), (75,100)]

-- func of Type Animation which is the left to right movement of lime coloured triangles - on different points
triLimeLTR :: Animation
triLimeLTR =
    (combine
    [translate 
    (always (fromIntegral 0, fromIntegral a)) -- one animation
    (translate
        (repeatSmooth (0,0) [(x, (hor,ver)) | x <- [0,2..80], hor <- [800,750..0], ver <- [0]]) -- returns type Varying
            (withPaint (always lime) 
                (rightTri))) |
              a <- [0,200..600]])

-- func of Type Animation which is the right to left movement of lime coloured triangles              
triLimeRTL :: Animation
triLimeRTL = 
    (combine
    [translate 
    (always (fromIntegral 0, fromIntegral a)) -- one animation
    (translate
        (repeatSmooth (0,0) [(x, (hor,ver)) | x <- [2,4], hor <- [0,50..800], ver <- [0]]) -- returns type Varying
            (withPaint (always lime) 
                (leftTri))) |
              a <- [0,200..600]])

-- func of type Animation which is the centre set of rotating triangles              
ctrlPiece :: Animation
ctrlPiece =
    polygon[(75,25), (125,25), (75,75), (125,75)]


-- func of type Animation which is right to left movment of teal triangles    
triTealRTL :: Animation
triTealRTL =
    (combine
    [translate 
    (always (fromIntegral 0, fromIntegral a)) -- one animation
    (translate
        (repeatSmooth (800,0) [(x, (hor,ver)) | x <- [1,3], hor <- [0,50..800], ver <- [0]]) -- returns type Varying
            (withPaint (always teal) 
                (leftTri))) |
              a <- [100,300..500]])

-- func of type Animation which is right to left movment of teal triangles                  
triTealLTR :: Animation
triTealLTR = 
    (combine
    [translate 
    (always (fromIntegral 0, fromIntegral a)) -- one animation
    (translate
        (repeatSmooth (800,0) [(x, (hor,ver)) | x <- [3,5], hor <- [800,750..0], ver <- [0]]) -- returns type Varying
            (withPaint (always teal) 
                (rightTri))) |
              a <- [100,300..500]])

-- (rotate (spinner 2) ctrlPiece)   test with this later            
 
test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)