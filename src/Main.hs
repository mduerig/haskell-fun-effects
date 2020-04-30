module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Free

-- A simple DSL
data Console a
    = WriteLn String ( Console a )
    | ReadLn ( String -> Console a )
    | Condition Bool ( Console a ) ( Console a )
    | Result a

-- A direct interpreter for the DSL in the IO monad
interpIO :: Console a -> IO a
interpIO ( WriteLn s c ) = putStrLn s >> interpIO c
interpIO ( ReadLn r ) = getLine >>= interpIO . r
interpIO ( Condition c t e ) = if c then interpIO t else interpIO e
interpIO ( Result v ) = pure v

-- A simple program encoded in the DSL directly
program1 :: Console String
program1 =
    WriteLn "What's your name?"
      ( ReadLn
        ( \name -> WriteLn ( "Hello " ++ name )
          ( Condition ( length name > 2 )
              ( Result "this is a long name" )
              ( Result "this is a short name" )
          )
        )
    )

interp1 :: IO String
interp1 = interpIO program1

-- Convenience functions for making the DSL easier to use
writeLn :: String -> Console ()
writeLn s = WriteLn s ( Result () )

readLine :: Console String
readLine = ReadLn Result

condition :: Bool -> Console Bool
condition c = Condition c ( Result True ) ( Result False )

result :: a -> Console a
result = Result

-- Turning the DSL into a monad
instance Functor Console where
    fmap f ( WriteLn s c ) = WriteLn s ( fmap f c )
    fmap f ( ReadLn r ) = ReadLn ( fmap f . r )
    fmap f ( Condition c t e ) = Condition c ( fmap f t ) ( fmap f e )
    fmap f ( Result x ) = Result ( f x )

instance Applicative Console where
    pure = Result
    (<*>) = ap

instance Monad Console where
    ( WriteLn s c ) >>= f = WriteLn s ( c >>= f )
    ( ReadLn r ) >>= f = ReadLn ( r >=> f )
    ( Condition c t e ) >>= f = Condition c ( t >>= f ) ( e >>= f )
    ( Result x ) >>= f = f x

-- Now the DSL can be used in monadic style
program2 :: Console String
program2 = do
    writeLn "Say your name please..."
    name <- readLine
    writeLn ( "Hello again " ++ name )
    c <- condition ( length name > 2 )
    if c
        then writeLn "This is a long name"
        else writeLn "This is a short name"
    result "Bye!"

interp2 :: IO String
interp2 = interpIO program2

-- De-sugared version
program3 :: Console String
program3 =
    writeLn "Name:" >>
    readLine >>= \name ->
    writeLn ( "Oi mate " ++ name ) >>
    condition ( length name > 2 ) >>=
        \c -> if c
            then result "you have a long name"
            else result "you have a short name"

interp3 :: IO String
interp3 = interpIO program3

----------------
-- Implementing the same DSL by using the free monad gives us the interpreter
-- for (almost) free. I.e. no need for explicit recursion.

-- The DSL again. Note how the type of the continuation changed from above: the
-- free monad takes care of the recursive step, no need to encode it ourselves.
data ConsoleF a
    = WriteLnF String a
    | ReadLnF ( String -> a )
    | ConditionF Bool a a
    | ResultF a

-- Turn the DSL into a functor. Alternatively we could use the DeriveFunctor
-- language extension to derive it.
instance Functor ConsoleF where
    fmap f ( WriteLnF s c ) = WriteLnF s ( f c )
    fmap f ( ReadLnF r ) = ReadLnF ( f . r )
    fmap f ( ConditionF c t e ) = ConditionF c ( f t ) ( f e )
    fmap f ( ResultF x ) = ResultF ( f x )

-- The free monad for our DSL
type ConsoleFM = Free ConsoleF

-- Again a few convenience functions
writeLnFM :: String -> ConsoleFM ()
writeLnFM s = liftF $ WriteLnF s ()

readLineFM :: ConsoleFM String
readLineFM = liftF $ ReadLnF id

conditionFM :: Bool -> ConsoleFM Bool
conditionFM c = liftF $ ConditionF c True False

resultFM :: a -> ConsoleFM a
resultFM x = liftF $ ResultF x

-- No need to implement ourselves...
-- liftF :: Functor f => f a -> Free f a
-- liftF = Free . fmap return

-- The free interpreter. Note how we only need to define the semantics
-- for the base cases. The recursive calls are taken care of by the free monad.
interpIOF :: ConsoleFM a -> IO a
interpIOF = foldFree alg
    where
        alg :: ConsoleF x -> IO x
        alg ( WriteLnF s c ) = c <$ putStrLn s
        alg ( ReadLnF r ) = r <$> getLine
        alg ( ConditionF c t e ) = if c then pure t else pure e
        alg ( ResultF x ) = pure x

-- Now we can use the new DSL in monadic style
program4 :: ConsoleFM Int
program4 = do
    writeLnFM "Say something"
    name <- readLineFM
    writeLnFM ( "You said: " ++ name )
    if length name > 2
        then writeLnFM "A long name again"
        else writeLnFM "A short name again"
    resultFM ( length name )

interp4 :: IO Int
interp4 = interpIOF program4

-- The DSL composes nicely
echo :: ConsoleFM ()
echo = do
    someThing <- readLineFM
    writeLnFM someThing

-- Using the composite
program5 :: ConsoleFM ()
program5 = do
    writeLnFM "I'm your echo"
    echo

interp5 :: IO ()
interp5 = interpIOF program5

-- Try that cruft
main :: IO ()
main = do
    print "Playing around with functional effects"
    print "Running program 1"
    s <- interp1
    print s
    print "-----------------"

    print "Running program 2"
    s <- interp2
    print s
    print "-----------------"

    print "Running program 3"
    s <- interp3
    print s
    print "-----------------"

    print "Running program 4"
    i <- interp4
    print i
    print "-----------------"

    print "Running program 5"
    interp5
    print "-----------------"
