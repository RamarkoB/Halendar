{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Model where

import Miso.String (MisoString, ms)

import DateTime
import Event

data Env where
  Env :: {getCss :: String} -> Env

-- Data for Viewing Mode
data Mode = Week | Month
  deriving (Eq, Show)

-- Data for Editing State
data State = View | Edit
  deriving (Eq, Show)

-- Data for Type of new Event
data EventType = NewAllDay | NewTask | NewEvent | NoEvent deriving (Eq, Show)

-- Data for storing input and constructing a new event
data InputEvent = InputEvent 
  { getEventType :: EventType
  , getInputName :: MisoString
  , getInputMonth :: MisoString
  , getInputDay :: MisoString
  , getInputYear :: MisoString
  , getHour1 :: MisoString
  , getMin1 :: MisoString
  , getMer1 :: Meridiem
  , getHour2 :: MisoString
  , getMin2 :: MisoString
  , getMer2 :: Meridiem
  } deriving (Eq, Show)

--Input Fields used for new Events
data InputField = N0 | M0 | D0 | Y0 | H1 | M1 | H2 | M2 deriving (Eq, Show)

-- Data for am vs pm
data Meridiem = AM | PM deriving (Eq, Show)

-- Intialize an InputEvent
newInput :: InputEvent
newInput = InputEvent NoEvent s s s s s s AM s s AM
  where s = ms ""

-- Update InputEvent on choosing an EventType
setInputEvent :: EventType -> InputEvent -> InputEvent
setInputEvent e ie = ie {getEventType=e}

-- Update InputEvent on input field edit
updateInput :: MisoString -> InputField -> InputEvent -> InputEvent
updateInput str N0 ie = ie {getInputName=str}
updateInput str D0 ie = ie {getInputDay=str}
updateInput str M0 ie = ie {getInputMonth=str}
updateInput str Y0 ie = ie {getInputYear=str}
updateInput str H1 ie = ie {getHour1=str}
updateInput str M1 ie = ie {getMin1=str}
updateInput str H2 ie = ie {getHour2=str}
updateInput str M2 ie = ie {getMin2=str}

-- Struct for overall model
data Model = Model
    { getToday :: Date
    , getNow :: Time
    , currDate :: Date
    , eventTree :: Tree Agenda
    , getMode :: Mode
    , getState :: State
    , getInputEvent :: InputEvent
    } deriving (Eq, Show)

-- Intialize a Model
initialModel :: Date -> Time -> Model
initialModel date time = Model date time date initialCal Week View newInput

-- Intialize Events on Calendar
initialCal :: Tree Agenda
initialCal = foldr (uncurry insert) EmptyTree eventList
  where
    eventList =
      [ (Date 12 1 2023, Task "Project Proposal Report Due" (Time 11 0))
      , (Date 12 2 2023, Event "Go Downtown!" (Time 16 0) (Time 18 0))
      , (Date 12 4 2023, Event "Project Presentation" (Time 10 0) (Time 12 0))
      , (Date 12 4 2023, AllDay "National Cookie Day")
      , (Date 12 5 2023, AllDay "International Volunteer Day")
      , (Date 12 5 2023, Event "Project Presentation" (Time 12 0) (Time 14 0))
      , (Date 12 5 2023, Event "Mark Suprise Party" (Time 13 0) (Time 13 30))
      , (Date 12 6 2023, AllDay "National Gazpacho Day")
      , (Date 12 6 2023, Event "Project Presentation" (Time 12 0) (Time 14 0))
      , (Date 12 7 2023, AllDay "National Letter Writing Day")
      , (Date 12 7 2023, Event "Project Presentation" (Time 12 0) (Time 14 0))
      , (Date 12 8 2023, Task "Colonizations III Essay" (Time 21 0))
      , (Date 12 8 2023, AllDay "National Brownie Day")
      , (Date 12 25 2023, AllDay "Christmas")
      ]