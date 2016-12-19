> import Data.List (zipWith4)
> import Prelude hiding (not, and, or)

-------------------------------------------------
                  Initial Setup
-------------------------------------------------

> data Bit = Zero | One
>   deriving (Eq, Show)

> type Signal = [Bit]
> type Signal4 = (Signal, Signal, Signal, Signal)

> clock :: Signal
> clock = Zero : One : clock

> zero, one :: Signal
> zero = Zero : zero
> one  = One  : one

> bitPairToSignal :: [(Bit, Bit)] -> (Signal, Signal)
> bitPairToSignal ((b1, b2):bitPairs) = (b1:bs1, b2:bs2)
>                                   where (bs1, bs2) = bitPairToSignal bitPairs


The interpreter turns a four bit Signal into a 'stream' of integers
given at: http://www.cs.yale.edu/homes/hudak/CS201S08/Programs/Circuits.txt

> interp :: Signal4 -> [Int]
> interp (a3, a2, a1, a0) =
>    zipWith4 (\a b c d -> (a + b + c + d))
>      (map (foo 1) a0) (map (foo 2) a1)
>      (map (foo 4) a2) (map (foo 8) a3)
>    where foo n b = if (b == One) then n else 0

-------------------------------------------------
              Boolean Operations
-------------------------------------------------

> not :: Bit -> Bit
> not Zero = One
> not One  = Zero

> and, or, nand, nor, xor :: Bit -> Bit -> Bit
> and  One  One  = One
> and  x    y    = Zero
> or   Zero Zero = Zero
> or   x    y    = One
> nand x y = not (and x y)
> nor  x y = not (or x y)
> xor One  Zero = One
> xor Zero One  = One
> xor x    y    = Zero

> notSignal :: Signal -> Signal
> notSignal (x:xs) = not x : notSignal xs

> logicToSignal :: (a -> b -> c) -> [a] -> [b] -> [c]
> logicToSignal op (x:xs) (y:ys) = op x y : logicToSignal op xs ys

> andSignal, orSignal, nandSignal, norSignal, xorSignal :: Signal -> Signal -> Signal
> andSignal  = logicToSignal and
> orSignal   = logicToSignal or
> nandSignal = logicToSignal nand
> norSignal  = logicToSignal nor
> xorSignal  = logicToSignal xor

> toSignal3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
> toSignal3 f (x:xs) (y:ys) (z:zs) = f x y z : toSignal3 f xs ys zs

> addSignal :: Signal -> Signal -> Signal -> (Signal, Signal)
> addSignal c_ins s1 s0 = bitPairToSignal (toSignal3 adder c_ins s1 s0)

--------------------------------------------------
                    Components
--------------------------------------------------

> halfAdder :: Bit -> Bit -> (Bit, Bit)
> halfAdder x y = (and x y, xor x y)

> adder :: Bit -> Bit -> Bit -> (Bit, Bit)
> adder c_in x y = (or c_out1 c_out2, s2)
>               where  (c_out1, s1) = halfAdder c_in x
>                      (c_out2, s2) = halfAdder s1  y


> dFlipFlop :: Signal -> Signal -> Signal
> dFlipFlop val clk = output
>     where output = Zero : orSignal (andSignal val clk)
>                      (andSignal output (notSignal clk))

> register :: Signal4 -> Signal -> Signal4
> register ~(d3, d2, d1, d0) clk =
>   (dFlipFlop d3 clk,
>    dFlipFlop d2 clk,
>    dFlipFlop d1 clk,
>    dFlipFlop d0 clk)

> add4 :: Signal4 -> Signal4 -> Signal -> (Signal, Signal4)
> add4 (a3, a2, a1, a0) (b3, b2, b1, b0) c_in = (c3, (s3, s2, s1, s0))
>    where (c0, s0) = addSignal a0 b0 c_in
>          (c1, s1) = addSignal a1 b1 c0
>          (c2, s2) = addSignal a2 b2 c1
>          (c3, s3) = addSignal a3 b3 c2

> mux :: Signal -> Signal -> Signal -> Signal
> mux as bs select = orSignal (andSignal as select)
>                      (andSignal bs (notSignal select))

> mux4 :: Signal4 -> Signal4 -> Signal -> Signal4
> mux4 (a3, a2, a1, a0) (b3, b2, b1, b0) select =
>   (mux a3 b3 select,
>    mux a2 b2 select,
>    mux a1 b1 select,
>    mux a0 b0 select)

-------------------------------------------------
          Example: A 4-bit Counter
-------------------------------------------------

> counter :: Signal -> Signal4
> counter clk = output
>            where output   = register s clk
>                  (c, s)   = add4 (zero, zero, zero, one) output zero

-------------------------------------------------
      Example: Scalar Addition Machine
-------------------------------------------------

> exampleMachine :: Signal4 -> Signal4 -> Signal -> Signal -> Signal4
> exampleMachine a b clk select = output
>    where output       = register result clk
>          (_ , result) = add4 a c zero
>          c            = mux4 b output select



-------------------------------------------------
            Tests for the Examples
-------------------------------------------------


> a, b :: Signal4
> a = (zero, zero,  one,  one)   
> b = (zero,  zero, zero, one)

> sel :: Signal
> sel = One : One : zero

> testCounter = take 22 (interp (counter clock))
> testExample = take 8 (interp (exampleMachine a b clock sel))
