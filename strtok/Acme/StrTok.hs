module Acme.StrTok (
    -- * The StrTokT monad transformer
    StrTokT,
    runStrTokT,
    -- * The StrTok monad
    StrTok,
    runStrTok,
    -- * The strTok function
    strTok
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Arrow
import Control.Monad.Trans


-- | The @StrTokT@ monad, parametrised with:
--
--   * @s@ - The type of list elements (e.g. @Char@ if the input to @strTok@ is a @String@).
--
--   * @m@ - The inner monad.
newtype StrTokT s m a = StrTokT { runStrTokT' :: [s] -> m (a, [s]) }

instance Functor m => Functor (StrTokT s m) where
  fmap f (StrTokT g) = StrTokT $ fmap (first f) . g
  
instance (Functor m, Monad m) => Applicative (StrTokT s m) where
  pure  = return
  (<*>) = ap
  
instance (Functor m, MonadPlus m) => Alternative (StrTokT s m) where
  empty = mzero
  (<|>) = mplus
    
instance (MonadPlus m) => MonadPlus (StrTokT s m) where
  mzero       = StrTokT $ const mzero
  m `mplus` n = StrTokT $ \s -> runStrTokT' m s `mplus` runStrTokT' n s

instance (MonadFix m) => MonadFix (StrTokT s m) where
  mfix f = StrTokT $ \s -> mfix $ \ ~(a, _) -> runStrTokT' (f a) s   

instance Monad m => Monad (StrTokT s m) where
  return x = StrTokT $ \s -> return (x,s)
  StrTokT f >>= g = StrTokT $ \s -> do {(x,s) <- f s; runStrTokT' (g x) s}

instance MonadTrans (StrTokT s) where
  lift m = StrTokT $ \s -> do {x <- m; return (x,s)}
  
instance (MonadIO m) => MonadIO (StrTokT s m) where
    liftIO = lift . liftIO

-- | Executes a @strTok@ computation in the state transformer monad @StrTokT@
runStrTokT :: Functor m => StrTokT s m a -> m a
runStrTokT (StrTokT f) = fmap fst (f [])



-- | The @StrTok@ monad
type StrTok s = StrTokT s Identity

-- | Executes a @strTok@ computation in the state monad @StrTok@
runStrTok :: StrTok s a -> a
runStrTok = runIdentity . runStrTokT



-- | A Haskell variant of the @strtok@ function from C and PHP. This function splits a string into tokens which are
-- delimited by a given set of characters. A call with @Just s@ and the delimiting characters @ds@ will yield 
-- the first token in @s@ that is delimited by characters from @ds@. Every subsequent call of @strTok@ with @Nothing@ 
-- will yield the next token. If the string contains no more tokens, an empty list is returned.
--
-- @strTok@ returns a stateful computation of type @StrTokT a m [a]@ (or @StrTok a [a]@). 
-- Several invocations of @strTok@ and computations with the results can be chained in the @StrTokT@ (resp. @StrTok@) 
-- monad and then executed with @runStrTokT@ (resp. @runStrTok@).
-- 
-- Example:
--
-- >runStrTokT $
-- >      do a <- strTok (Just "- This, a sample string.") " ,.-"
-- >         b <- strTok Nothing " ,.-"
-- >         c <- strTok Nothing ",.-"
-- >         return (a, b, c)
--
-- evaluates to
--
-- >("This","a"," sample string")
strTok :: (Eq a, Monad m) => Maybe [a] -> [a] -> StrTokT a m [a]
strTok s delims = StrTokT $ maybe strTok' (const . strTok') s
  where strTok' = return . break (`elem` delims) . dropWhile (`elem` delims)


