{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Sometimes you need to care about the path you take.
--
-- Suppose you're parsing some data -- you're likely using the 'Either'
-- type to keep track of errors. This is good!
--
-- Unfortunately, sometimes you have weird nested data, and then when you
-- go to parse, you get this back:
--
-- @
-- 'Left' (IntParseError "c")
-- @
--
-- Now, you're left wondering: "Where is that @c@? How did it get there?
-- How can I minimally reproduce this?" And unfortunately, all of the
-- context is lost: the error has been thrown from deep in the stack, and
-- you're stuck munging around in the source data.
--
-- When you're dealing with some deeply nested data that might fail, you're
-- left wondering: How do I get there from here? Let's leave breadcrumbs
-- along the way, so that we can find our way back!
--
-- Suppose you're trying to find a specific value in a deeply nested data
-- structure. You can use 'Bread' to lay breadcrumbs and then 'exit' as
-- soon as you've found what you need, collecting the breadcrumbs along the
-- way.
module Control.Monad.Bread where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Identity

-- | 'BreadT' is a monad transformer that allows you to leave breadcrumbs
-- of types @crumb@ while you're performing some effects on the underlying
-- monad @m@. If you reach an error condition, then @'throwError' exit@ will
-- end up returning a @'Left' (crumbs ':!:' exit)@, where @crumbs@ is a list of
-- all breadcrumbs that you've left so far.
--
-- @since 0.1.0.0
newtype BreadT crumb exit m a
    = BreadT
    { unBreadT :: ReaderT [crumb] (ExceptT ([crumb], exit) m) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState s
        , MonadIO
        , MonadWriter w
        )

-- | Unwrap the 'BreadT' part of a monad transformer stack. The return type
-- contains 'Either' a pair of the breadcrumbs and the thrown error, or
-- the result value.
--
-- @since 0.1.0.0
runBreadT
    :: BreadT crumb exit m a
    -> m (Either ([crumb], exit) a)
runBreadT = runExceptT . flip runReaderT [] . unBreadT

-- | A monad that can collect breadcrumbs and exit early with them, but do
-- nothing else.
--
-- @since 0.1.0.0
type Bread crumb exit = BreadT crumb exit Identity

-- | Run a 'Bread' computation, returning either a pair of the breadcrumbs
-- and error or a successful result.
--
-- @since 0.1.0.0
runBread :: Bread crumb exit a -> Either ([crumb], exit) a
runBread = runIdentity . runBreadT

-- | Lay a breadcrumb, so you'll know where you came from.
--
-- >>> runBread (withCrumb 'a' (exit "Nope"))
-- Left ("a", "Nope")
--
-- @since 0.1.0.0
withCrumb
    :: Monad m
    => crumb
    -> BreadT crumb exit m a
    -> BreadT crumb exit m a
withCrumb crumb = BreadT . local (crumb :) . unBreadT

-- | Short circuit the 'BreadT' computation. This causes the computation to
-- exit with the provided @exit@ value and the @crumb@s collected along the
-- way.
--
-- @since 0.1.0.0
exit :: Monad m => exit -> BreadT crumb exit m a
exit err =
    BreadT
        $ ReaderT
        $ \crumb -> ExceptT
        $ pure
        $ Left
        $ (crumb, err)

-- | Sometimes, a 'BreadT' computation short-circuits with an 'exit', but
-- you don't want it to 'exit' just yet -- perhaps you want to take
-- a different path. This function lets you handle 'exit' and potentially
-- choose a different path.
--
-- @
-- 'withCrumb' 1 $ do
--      exit "I'm tired"
--          `handleExit` \reason ->
--              pure "No, let's persevere!"
-- @
--
-- @since 0.1.0.0
handleExit
    :: Monad m
    => BreadT crumb exit m a
    -> (exit -> BreadT crumb exit m a)
    -> BreadT crumb exit m a
handleExit action cb =
    BreadT
        $ ReaderT
        $ \crumb -> catchError
            (runReaderT (unBreadT action) crumb)
            (flip runReaderT crumb . unBreadT . cb . snd)

-- | Return the current collection of crumbs collected thus far.
--
-- @since 0.1.0.0
crumbs :: Monad m => BreadT crumb exit m [crumb]
crumbs = BreadT ask

-- | The 'MonadReader' instance for 'BreadT' delegates to the underlying
-- instance for @m@.
instance MonadReader r m => MonadReader r (BreadT crumb exit m) where
    ask = BreadT (lift ask)
    local f (BreadT (ReaderT crumb'ema)) =
        BreadT . ReaderT $ local f . crumb'ema

-- | The 'MonadError' instance for 'BreadT' gathers the @crumb@otations
-- collected so far, and throws the exception paired with the crumbotations.
instance (Monad m) => MonadError exit (BreadT crumb exit m) where
    throwError = exit
    catchError = handleExit

instance  MonadTrans (BreadT crumb exit) where
    lift = BreadT . lift . lift
