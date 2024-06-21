module Event where

import DateTime

-------------------------------------------------------------------------------
-- Event and Event Functions

-- Event Struct to store types of events
data Event = AllDay String | Task String Time | Event String Time Time
    deriving Show

instance Eq Event where
    (==) e1 e2 = check e1 == check e2
        where
            check (AllDay _)= Time 0 0
            check (Task _ t) = t
            check (Event _ t _) = t

instance Ord Event where
    (<=) e1 e2 = check e1 <= check e2
        where
            check (AllDay _)= Time 0 0
            check (Task _ t) = t
            check (Event _ t _) = t

-- Get length of event
eventLength :: Time -> Time -> Time
eventLength (Time hr1 min1) (Time hr2 min2)
    | min2 < min1 =  eventLength (Time hr1 min1) (Time (hr2 - 1) (min2 + 60))
    | otherwise = Time (hr2 - hr1) (min2 - min1)

-- Check if event is an AllDay event
isAllDay :: Event -> Bool
isAllDay (AllDay _) = True
isAllDay _ = False

-- Filter for AllDay events
getAllDay :: [Event] -> [Event]
getAllDay = filter isAllDay

-- Filter for non-AllDay events
getNotAllDay :: [Event] -> [Event]
getNotAllDay = filter (not . isAllDay)

-------------------------------------------------------------------------------
-- Agenda and Tree Agenda Functions

-- Agenda struct stores date and all corresponding events
data Agenda = Agenda { getDate :: Date, getEvents :: [Event]}
    deriving (Eq, Show)

-- Tree for Agendas
data Tree a = Node a (Tree a) (Tree a) | EmptyTree
    deriving (Eq, Show)

-- Inserts a new event into an Agenda Tree
insert :: Date -> Event -> Tree Agenda -> Tree Agenda
insert d e (Node n s1 s2)
    |  d < getDate n && s1 == EmptyTree = Node n (insert d e EmptyTree) s2
    |  d > getDate n && s2 == EmptyTree = Node n s1 (insert d e EmptyTree)
    |  d < getDate n = Node n (insert d e s1) s2
    |  d > getDate n = Node n s1 (insert d e s2)
    |  otherwise = Node (addEvent e n) s1 s2
insert d e EmptyTree = Node (Agenda d [e]) EmptyTree EmptyTree

-- Inserts a new event into an Agenda
addEvent :: Event -> Agenda -> Agenda
addEvent e (Agenda d es) = Agenda d (insertEvent es)
    where
        insertEvent [] = [e]
        insertEvent (x:xs) = if e < x then e : x : xs else x : insertEvent xs

-- Gets Agenda for a Date from Agenda Tree
getAgenda :: Date -> Tree Agenda -> Agenda
getAgenda d EmptyTree = Agenda d []
getAgenda d (Node n s1 s2)
    | d < getDate n =  getAgenda d s1
    | d > getDate n = getAgenda d s2
    | otherwise = Agenda d (getEvents n)