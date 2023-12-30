-- Recall the data type

-- data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
--			   deriving (Show, Read, Eq, Ord, Enum)

-- defined in the Lecture Notes.  Define a new data type which represents just the working days of the week.  Show that it is a retract of the above type.

data WorkingDay = Mon' | Tue' | Wed' | Thu' | Fri'
  deriving (Eq, Show)

toWeekDay :: WorkingDay -> WeekDay
toWeekDay Mon' = Mon
toWeekDay Tue' = Tue
toWeekDay Wed' = Wed
toWeekDay Thu' = Thu
toWeekDay Fri' = Fri

toWorkingDay :: WeekDay -> WorkingDay
toWorkingDay Mon = Mon'
toWorkingDay Tue = Tue'
toWorkingDay Wed = Wed'
toWorkingDay Thu = Thu'
toWorkingDay Fri = Fri'
toWorkingDay Sat = Fri'
toWorkingDay Sun = Mon'