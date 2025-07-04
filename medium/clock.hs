-- Implement a clock that handles times without dates.
-- https://exercism.org/tracks/haskell/exercises/clock

module Clock (addDelta, fromHourMin, toString) where
data Clock = Clock Int Int
  deriving Eq
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = validClock (Clock hour min)
toString :: Clock -> String
toString clock = addZero h ++ ":" ++ addZero m
          where addZero x = (if x < 10 then "0" else "") ++ show x
                (Clock h m) = validClock clock
addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = validClock (Clock (hour + h) (min + m))
validClock :: Clock -> Clock
validClock (Clock h m) = Clock newH newM
                     where newM = mod m 60
                           newH = mod (h + (div m 60)) 24