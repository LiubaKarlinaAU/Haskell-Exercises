-- Determine wheather a given year is a leap year.
-- https://exercism.org/tracks/haskell/exercises/leap

module LeapYear (isLeapYear) where
isLeapYear :: Integer -> Bool
isLeapYear year = if isDividedBy 4 
                  then  
                    if isDividedBy 100 
                    then isDividedBy 400
                    else True
                  else False
      where isDividedBy n = year `mod` n == 0 