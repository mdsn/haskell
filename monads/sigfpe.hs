-- You could have invented monads


-- Start with simple, composable functions
f :: Float -> Float
f = sqrt

g :: Float -> Float
g x = 1/(sqrt x)

-- The need arises to return something along
-- We'll call them "debuggable functions"
-- The price: functions are no longer composable
f' :: Float -> (Float, String)
f' x = (f x, "f was called")

g' :: Float -> (Float, String)
g' x = (g x, "g was called")

-- We want composed debuggable functions to return
-- the composition of its functions and the 
-- concatenation of the strings

gof :: Float -> (Float, String)
gof x = let (y, s) = g' x
            (z, t) = f' y in (z, s++t)

-- It works, but it's too much plumbing
-- Write bind to abstract it away
bind :: (Float -> (Float, String)) ->
        (Float, String) ->
        (Float, String)
bind f' (gx, gs) = let (fx, fs) = f' gx
                   in (fx, gs++fs)

-- gof' 3 = bind f' (g' 3)
-- gof'   = bind f' . g'

-- Now, define unit. Properties:
--  f . id = f
--  id . f = f

unit :: Float -> (Float, String)
unit x = (x, "")

-- unit allows shoving a value into the monad
-- unit 3 = (3.0, "")
-- bind f' (unit 3) = (1.732.., "f was called")
-- bind g' (bind f' (unit 3)) = gof' 3

-- unit also allows lifting a function as a
-- as a debuggable one

lift :: (a -> Float) -> a -> (Float, String)
lift f = unit . f

-- lift f 3 = (1.73.., "")
-- bind g' (lift f 3) = (0.75.., "g was called")
-- lift (f . g) 3 = (0.75.., "")
