{- 
    All about monads - 5: exercises
    http://www.haskell.org/haskellwiki/All_About_Monads#Exercises
-}
import Control.Monad (mplus)

data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

instance Show Sheep where
    show s = show (name s)

-- Remember the Maybe monad definition:
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing
--    (Just x) >>= f = f x
--    return         = Just

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do m <- mother s
                           father m

maternalGrandfather' :: Sheep -> Maybe Sheep
maternalGrandfather' s = mother s >>= \m -> father m

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
                                  mother gm


mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m <- mother s
                                  gf <- father m
                                  father gf


-- Exercise 1: rewrite without using do notation

fathersMaternalGrandmother' :: Sheep -> Maybe Sheep
fathersMaternalGrandmother' s =
    father s >>= \f ->
    mother f >>= \gm ->
    mother gm

mothersPaternalGrandfather' :: Sheep -> Maybe Sheep
mothersPaternalGrandfather' s =
    mother s >>= \m ->
    father m >>= \gf ->
    father gf


-- Exercise 2: write parent and grandparent using mplus

parent' :: Sheep -> Maybe Sheep
parent' s = (mother s) `mplus` (father s)

grandparent :: Sheep -> Maybe Sheep
grandparent s = (mother s >>= parent') `mplus` (father s >>= parent')

breedSheep :: Sheep
breedSheep = let adam   = Sheep "Adam" Nothing Nothing
                 eve    = Sheep "Eve" Nothing (Just adam)
                 uranus = Sheep "Uranus" Nothing Nothing
                 gaea   = Sheep "Gaea" Nothing Nothing
                 kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                 holly  = Sheep "Holly" (Just eve) (Just adam)
                 roger  = Sheep "Roger" (Just eve) (Just kronos)
                 molly  = Sheep "Molly" (Just holly) (Just roger)
            in Sheep "Dolly" (Just molly) Nothing

main :: IO ()
main = let dolly = breedSheep
       in do print (mothersPaternalGrandfather' dolly)
