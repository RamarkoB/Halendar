{-# LANGUAGE FlexibleInstances #-}

module DateTime where

import Data.Time

-- Helper Function
if' :: Bool -> a -> a -> a
if' True v _ = v
if' False _ v = v

-------------------------------------------------------------------------------
-- Date and Time Structs

-- Struct to store Dates
data Date = Date
                { getMonth :: Int
                , getDay :: Int
                , getYear :: Int }
                deriving (Eq)

instance Ord Date where
    (<=) d1 d2 = if' (getYear d1 <= getYear d2)
        True (if' (getMonth d1 <= getMonth d2)
            True (getDay d1 <= getDay d2))

instance Show Date where
    show (Date m d y) = show m ++ "/" ++ show d ++ "/" ++ show y

-- Struct to store Times
data Time = Time
                { getHour ::Int
                , getMinute :: Int }
                deriving (Eq, Ord)

instance Show Time where
    show (Time h m) = show h ++ ":" ++ show m

-- Constructs Date from current time
today :: IO Date
today = fmap ((\(y, m, d) -> Date m d (fromIntegral y)) . (toGregorian . localDay  . zonedTimeToLocalTime)) getZonedTime

-- Constructs Time from current time
rightNow :: IO Time
rightNow = fmap ((\(TimeOfDay hr minute _) -> Time hr minute) . localTimeOfDay . zonedTimeToLocalTime) getZonedTime

-- Handles overflowing dates to real dates
toDate :: Int -> Int -> Int -> Date
toDate m d y
    | d > daysInMonth m y = toDate nm (d - daysInMonth m y) ny
    | d < 1 = toDate pm (daysInMonth pm py + d) py
    | otherwise = Date m d y
    where
        (nm, ny) = nextMonth (m, y)
        (pm, py) = prevMonth (m, y)

-- Check if a date is valid
checkValidDate :: Int -> Int -> Int -> Maybe Date
checkValidDate m d y
    | m < 0 || m > 12  = Nothing
    | d < 0 || d > daysInMonth m y = Nothing
    | otherwise = Just $ Date m d y

-------------------------------------------------------------------------------
-- Day of Week Functions

-- check if a year is a Leap Year
leapYear :: Int -> Bool
leapYear y = (mod y 400 == 0) || ((mod y 4 == 0) && mod y 100 /= 0)

-- Value needed for Weekday Calculation
dayBump :: Int -> Int -> Int
dayBump 1 y = if' (leapYear y) 0 1
dayBump 2 y = if' (leapYear y) 3 4
dayBump 3 _ = 4
dayBump 4 _ = 0
dayBump 5 _ = 2
dayBump 6 _ = 5
dayBump 7 _ = 0
dayBump 8 _ = 3
dayBump 9 _ = 6
dayBump 10 _ = 1
dayBump 11 _ = 4
dayBump 12 _ = 6
dayBump _ _ = 0

-- Get Weekday of a day (0-Sunday, 1-Monday ... 6-Saturday)
weekday :: Date -> Int
weekday (Date m d y ) = mod (y - 1900 + dayBump m y + d + div y 4) 7

-------------------------------------------------------------------------------
-- Month Functions

-- Get Weekday of first day in month
firstOfMonth :: Int -> Int -> Int
firstOfMonth m y =  weekday $ Date m 1 y

-- Get number of days in month
daysInMonth :: Int -> Int -> Int
daysInMonth m y
    | m == 2 = if' (leapYear y) 29 28
    | m `elem` [9, 4, 6, 11] = 30
    | otherwise = 31

-------------------------------------------------------------------------------
-- Change Date

-- Get Next Day
next :: Date -> Date
next (Date 12 31 y) = Date 1 1 (y + 1)
next (Date m d y)
    | d == daysInMonth m y = Date (m + 1) 1 y
    | otherwise = Date  m (d + 1)  y

-- Get Previous Day
prev :: Date -> Date
prev (Date 1 1 y) = Date 12 31 (y - 1)
prev (Date m d y)
    | d == 1 = Date (m - 1) (daysInMonth (m - 1) y) y
    | otherwise = Date  m (d - 1) y

-- Move Date through Time
move :: Date -> Int -> Date
move date i
    | i > 0 = move (next date) (i - 1)
    | i < 0 = move (prev date) (i + 1)
    | otherwise = date

-- Get Next Week
nextWeek :: Date -> Date
nextWeek date = move date 7

-- Get Previous Week
prevWeek :: Date -> Date
prevWeek date = move date (-7)

-- Class Type to handle prevMonth and nextMonth for Date and (Int, Int)
class Monthable a where
    -- Get Previous Month
    prevMonth :: a -> a

    -- Get Next Month
    nextMonth :: a -> a

instance Monthable Date where
    prevMonth (Date 1 d y) = Date 12 (min d 31) (y - 1)
    prevMonth (Date m d y) = Date (m - 1) (min d (daysInMonth (m - 1) y)) y

    nextMonth (Date 12 d y) = Date 1 (min d 31) (y + 1)
    nextMonth (Date m d y) = Date (m + 1) (min d (daysInMonth (m + 1) y)) y

instance Monthable (Int, Int) where
    prevMonth (1, y) = (12, y - 1)
    prevMonth (m, y) = (m - 1, y)

    nextMonth (12, y) = (1, y + 1)
    nextMonth (m, y) = (m + 1, y)

-------------------------------------------------------------------------------
-- String Functions

monthToStr :: Int -> String
monthToStr n = ["January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December"] !! (n - 1)

weekdays :: [String]
weekdays = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]

weekToStr :: Int -> String
weekToStr n = weekdays !! n

calMonth :: Date -> String
calMonth (Date m _ y) = monthToStr m ++ " " ++ show y

-------------------------------------------------------------------------------
-- Generate List Functions

genWeek :: Date -> [Date]
genWeek date = [move date (i - weekday date) | i <- [0 .. 6]]

genMonth :: Date -> [Date]
genMonth (Date m _ y) = concat
    [genWeek (move first (i * 7)) | i <- [0 .. 5], 1 + (i * 7) <= weekday first + daysInMonth m y]
    where first = Date m 1 y