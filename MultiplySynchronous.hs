module MultiplySynchronous where

import CLaSH.Prelude
import CLaSH.Signal.Explicit (systemClock)
import CLaSH.Signal.Internal

multiplySigned' :: SClock clk
                -> Signal' clk (Signed n)
                -> Signal' clk (Signed m)
                -> Signal' clk (Signed (n+m))
multiplySigned' clk (a :- as) (b :- bs) = times a b :- multiplySigned' clk as bs
{-# NOINLINE multiplySigned' #-}

multiplySigned :: Signal (Signed n)
               -> Signal (Signed m)
               -> Signal (Signed (n+m))
multiplySigned = multiplySigned' systemClock

topEntity :: Signal (Signed 8) -> Signal (Signed 8) -> Signal (Signed 8)
topEntity a b = fmap resize $ delay $ delay $ c
  where
    a' = delay a
    b' = delay b
    c  = multiplySigned a' b'
    delay = register undefined
