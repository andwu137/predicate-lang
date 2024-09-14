module Text.Parser.Combinators (
    (<:>),
    surround,
    chainl1,
    chainl,
    chainr1,
    chainr,
) where

import Control.Applicative (Alternative ())
import Text.Megaparsec

infixr 5 <:>
(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

surround :: (Applicative f) => f b -> f a -> f a
surround b p = b *> p <* b

chainl1 :: (Monad m, Alternative m) => m a -> m (a -> a -> a) -> m a
chainl1 p = chainl p p

chainl :: (Monad m, Alternative m) => m a -> m a -> m (a -> a -> a) -> m a
chainl p1 p o =
    p1 >>= rest
  where
    rest l =
        (($ l) <$> o <*> p >>= rest)
            <|> pure l

chainr1 :: (Monad m, Alternative m) => m a -> m (a -> a -> a) -> m a
chainr1 p = chainr p p

chainr :: (Monad m, Alternative m) => m a -> m a -> m (a -> a -> a) -> m a
chainr p1 p o =
    p1 >>= rest
  where
    rest l =
        (($ l) <$> o <*> (p >>= rest))
            <|> pure l
