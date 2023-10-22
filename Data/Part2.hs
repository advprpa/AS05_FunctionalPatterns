{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

instance Applicative f => Functor f where 
    -- fmap in terms of Applicative
    fmap :: (a -> b) -> f a -> f b
    fmap f fa = pure f <*> fa

instance Monad f => Applicative f where
    -- pure in terms of Monad
    pure :: a -> f a
    pure = return

    -- (<*>) in terms of Monad
    (<*>) :: f (a -> b) -> f a -> f b
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)

