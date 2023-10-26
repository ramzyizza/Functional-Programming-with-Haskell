data Classification = Low | Medium | High | SuperHigh deriving (Show)

gasUsage :: (Fractional a, Ord a) => a -> Classification
gasUsage g  | g < 3      = Low
            | 3 <= g < 5 = Medium
            | 5 <= g < 7 = High
            | 7 <= g     = SuperHigh