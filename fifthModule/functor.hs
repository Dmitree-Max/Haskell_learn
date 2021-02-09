module Demo where



data Point3D a = Point3D a a a deriving Show
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
        deriving Show
data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show


instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)



instance Functor GeomPrimitive where
    fmap f (Point x) = Point $ fmap f x
    fmap f (LineSegment x y) = LineSegment (fmap f x) (fmap f y) 


instance Functor Tree where
    fmap f (Leaf x) = Leaf (fmap f x)
    fmap f (Branch l x r) = Branch (fmap f l) (fmap f x) (fmap f r)
