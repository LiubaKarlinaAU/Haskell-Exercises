-- Clean up user-entered phone numbers so that they can be sent SMS messages.
-- https://exercism.org/tracks/haskell/exercises/phone-number

module Phone (number) where
number :: String -> Maybe String
number xs = if len1 /= 11 || head cleaned1 /= '1' || cleaned1 !! 1 < '2' || cleaned1 !! 4 < '2'
                     then Nothing
                     else Just $ tail cleaned1 
       where cleaned = filter (\x -> x >= '0' && x <= '9') xs  
             len = length cleaned
             cleaned1 = if len == 10 then '1' : cleaned else cleaned
             len1 = length cleaned1