-- Given a decimal number, convert it to the appropriate sequence of events for a secret handshake.
-- https://exercism.org/tracks/haskell/exercises/secret-handshake

module SecretHandshake (handshake) where
handshake :: Int -> [String]
handshake n = code5 -- L.intercalate "," code5
            where b = bits n 4
                  actions = ["jump", "close your eyes", "double blink", "wink"] 
                  code4 = map (\(x,y) -> x) . filter (\(x,y) -> y) . zip actions $ tail b
                  code5 = if head b then code4 else reverse code4
bits :: Int -> Int -> [Bool]
bits n 0 = if n >= 1 then [True] else [False]
bits n k = bigger : bits rest (k-1)
         where bigger = n >= 2^k
               rest = if bigger then n - 2^k else n