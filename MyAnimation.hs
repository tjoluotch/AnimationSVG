module MyAnimation where

import Animation

-- remeber (0,0) otherwise known as the origin is always at the top left of the picture
picture :: Animation

-- picture = withPaint (always blue) (combine [translate (always (50, 50*i)) (rect (always (50*i)) (always 40)) | i <- [1..10]])


picture = 
    (translate
        (cycleSmooth 0.5 tupleA)
            (withPaint (always lime) 
                (rightTri)))

 -- List Function that increments from 0 up to 600 in 50's               
listA :: [Length]
listA = [0,50..800]

-- Use listA values and wrap these in a tuple with 0 values
tupleA ::[(Length, Length)]
tupleA = [(hor,vert) | hor <- listA, vert <- [0]]

-- function of type polygon which is a right facing triangle
rightTri :: Animation
rightTri = polygon [(0,0), (75,50), (0,100)]


test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)