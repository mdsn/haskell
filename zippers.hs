-- zippers.hs

data Node a = DeadEnd a
            | Passage a (Node a)
            | Fork    a (Node a) (Node a) deriving (Show)

type Zipper a = (Thread a, Node a)

data Branch a = KeepStraight a
              | TurnLeft a (Node a)
              | TurnRight a (Node a) deriving (Show)

type Thread a = [Branch a]

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
turnRight _               = Nothing

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Fork x l r) = Just (TurnLeft x r : t, l)
turnLeft _               = Nothing

keepStraight :: Zipper a -> Maybe (Zipper a)
keepStraight (t, Passage x n) = Just (KeepStraight x : t, n)
keepStraight _                = Nothing

get :: Node a -> a
get (DeadEnd x)     = x
get (Passage x _)   = x
get (Fork x _ _)    = x

put :: a -> Node a -> Node a
put x (DeadEnd _)   = DeadEnd x
put x (Passage _ a) = Passage x a
put x (Fork _ a b)  = Fork x a b

labyrinth :: Node (Int, Int)
labyrinth = Fork (0, 0)
                 (Fork (-1, 1)
                       (DeadEnd (-2, 2))
                       (DeadEnd (0, 2)))
                 (Passage (1, 1)
                          (Fork (1, 2)
                                (Passage (0, 3)
                                         (DeadEnd (0, 4)))
                                (DeadEnd (2, 3))))
