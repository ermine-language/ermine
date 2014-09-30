module Random where

{- ^ Generating random numbers in Ermine is not like generating random
numbers in most programming languages.  Because Ermine demands
referential transparency, it is nonsensical to expect an expression
like 'getRandom' to yield a different result each time it is
evaluated.  For further introduction, see
http://learnyouahaskell.com/input-and-output#randomness

To preserve referential transparency, this library makes explicit some
ideas that are usually implicit in random libraries.  For example, a
seeded random stream with no other entropy sources, given the seed
'n', will always yield the same infinite series of "random" values;
this is explicit in the type of 'randomInts'. -}

import IO
import IO.Unsafe using unsafePerformIO
import List.Stream

randomInts : Long -> Stream Int
randomInts = randomThings nextInt#

private
  randomThings : (Random# -> IO a) -> Long -> Stream a
  randomThings iof l =
    let r = fromSeed# l
        rec !i = i <:> rec (unsafePerformIO (iof r))
    in rec (unsafePerformIO (iof r))

private foreign
  data "java.util.Random" Random#
  constructor fromSeed# : Long -> Random#
  method "nextInt" nextInt# : Random# -> IO Int
