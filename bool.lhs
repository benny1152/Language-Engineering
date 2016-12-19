> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE UndecidableInstances #-}

> data Expr = Literal Bool | UnaryOp Unary Expr | BinaryOp Binary Expr Expr
> data Binary = AND | OR | XOR | IMPLY
> data Unary = ID | NOT

> binaryHandler :: Binary -> Bool -> Bool -> Bool
> binaryHandler AND = (&&)
> binaryHandler OR = (||)
> binaryHandler XOR = xor
>     where xor True False = True
>           xor False True = True
>           xor _ _ = False
> binaryHandler IMPLY = imply
>     where imply True False = False
>           imply _ _ = True

> unaryHandler :: Unary -> Bool -> Bool
> unaryHandler ID = id
> unaryHandler NOT = not

> evaluate :: Expr -> Bool
> evaluate (Literal x) = x
> evaluate (BinaryOp op x y) = (binaryHandler op) (evaluate x) (evaluate y)
> evaluate (UnaryOp op x) = (unaryHandler op) (evaluate x)

> data Expr' k = Literal' Bool | UnaryOp' Unary k | BinaryOp' Binary k k

> data Fix f = In (f (Fix f))

> inop :: Fix f -> f (Fix f)
> inop (In x) = x

> cata :: Functor f => (f a -> a) -> Fix f -> a
> cata alg = alg . fmap (cata alg) . inop

> instance Functor Expr' where
>   fmap f (Literal' bool) = Literal' bool
>   fmap f (UnaryOp' unary x) = UnaryOp' unary (f x)
>   fmap f (BinaryOp' binary x y) = BinaryOp' binary (f x) (f y)

> evaluate' :: Fix Expr' -> Bool
> evaluate' = cata alg where
>   alg :: Expr' Bool -> Bool
>   alg (Literal' x) = x
>   alg (BinaryOp' op x y) = (binaryHandler op) x y
>   alg (UnaryOp' op x) = (unaryHandler op) x
