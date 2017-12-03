module Chapter6 where

data Trivial = Trivial'
instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
-- day of week and numerical day of month
data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False



