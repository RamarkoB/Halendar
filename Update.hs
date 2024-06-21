{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Update where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import Miso
import Miso.String (fromMisoString, fromMisoStringEither, MisoString, ms)

import DateTime
import Event
import Model

-------------------------------------------------------------------------------
-- Action Defitions

runAction :: JSM action -> model -> Effect action model
runAction =
  (#>)
  -- https://hackage.haskell.org/package/miso-1.8.2.0/docs/Miso-Effect.html

data Action
  = NoOp
  | OnLoad
  | KeyPress Char
  | ArrowPress Direction
  | TimeUpdate Time
  | ViewUpdate
  | EventUpdate EventType
  | InputUpdate MisoString InputField
  | AddEventUpdate
  | MeridiemUpdate Int Meridiem
      deriving (Show, Eq)

data Direction
  = U
  | D
  | L
  | R
  | None
  deriving (Show, Eq)

-- Convert Key Input to an action
keysToAction :: Set Int -> Action
keysToAction setInt =
  case Set.toList setInt of
    [127] -> KeyPress '\DEL'
    [8]   -> KeyPress '\DEL'   -- '\b'
    [10]  -> KeyPress '\n'
    [13]  -> KeyPress '\n'     -- '\r'
    [i]   -> KeyPress $ chr i
    _     -> NoOp

-- Convert Arrow Input to an action
arrowsToAction :: Arrows -> Action
arrowsToAction a = ArrowPress $ toDirection a

-- Convert arrows into directions
toDirection :: Arrows -> Direction
toDirection Arrows {..} =
  case (arrowX, arrowY) of
    (-1, 0) -> L
    (1, 0) -> R
    (0, -1) -> D
    (0, 1) -> U
    _ -> None

-------------------------------------------------------------------------------
-- Model Update Handling

updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    NoOp                  -> noEff m
    OnLoad                -> noEff m
    KeyPress c            -> noEff $ keyUpdateModel c m
    ArrowPress d          -> noEff $ arrowUpdateModel d m
    TimeUpdate t          -> noEff $ timeUpdateModel t m
    ViewUpdate            -> noEff $ stateUpdateModel m
    EventUpdate e         -> noEff $ setEventUpdateModel e m
    InputUpdate s f       -> noEff $ inputUpdateModel s f m
    AddEventUpdate        -> noEff $ addEventUpdateModel m
    MeridiemUpdate i mer  -> noEff $ meridiemUpdateModel i mer m

every :: Int -> (Time -> action) -> Sub action
every n f sink = void . liftIO . forkIO . forever $ do
  threadDelay n
  res <- f <$> rightNow
  liftIO $ sink res

-- Handles Key Press Updates to Model
keyUpdateModel :: Char -> Model -> Model
keyUpdateModel _ m@(Model _ _ _ _ _ Edit _) = m
keyUpdateModel 'W' m = m {getMode = Month}
keyUpdateModel 'A' m = m {getMode = Week}
keyUpdateModel 'S' m@Model {..}
  = m {currDate = if' (getMode == Week) (prevWeek currDate) (prevMonth currDate)}
keyUpdateModel 'D' m@Model {..}
  = m {currDate = if' (getMode == Week) (nextWeek currDate) (nextMonth currDate)}
keyUpdateModel _ m = m

-- Handles Arrow Press Updates to Model
arrowUpdateModel :: Direction -> Model -> Model
arrowUpdateModel _ m@(Model _ _ _ _ _ Edit _) = m
arrowUpdateModel U m = m {getMode = Month}
arrowUpdateModel D m = m {getMode = Week}
arrowUpdateModel L m@Model {..}
  = m {currDate = if' (getMode == Week) (prevWeek currDate) (prevMonth currDate)}
arrowUpdateModel R m@Model {..}
  = m {currDate = if' (getMode == Week) (nextWeek currDate) (nextMonth currDate)}
arrowUpdateModel _ m = m

-- Handle Time Updates using Time struct (Unused)
timeUpdateModel :: Time -> Model -> Model
timeUpdateModel newTime m = m {getNow = newTime}

-- Handles switches between viewing and editing states
stateUpdateModel :: Model -> Model
stateUpdateModel m@Model {..}
  | getState == View = m {getState=Edit}
  | getState == Edit = m {getState=View, getInputEvent=newInput}
stateUpdateModel m = m

-- Handles selecting event to add
setEventUpdateModel :: EventType -> Model -> Model
setEventUpdateModel e m@Model {..} = m {getInputEvent = setInputEvent e getInputEvent}

-- Handles selecting am or pm for time inputs
meridiemUpdateModel :: Int -> Meridiem -> Model -> Model
meridiemUpdateModel 1 mer m@Model {..} = m {getInputEvent = getInputEvent {getMer1=mer}}
meridiemUpdateModel 2 mer m@Model {..} = m {getInputEvent = getInputEvent {getMer2=mer}}
meridiemUpdateModel _ _ m = m 

-- Handles adding a new event to the model
addEventUpdateModel :: Model -> Model
addEventUpdateModel m@Model {..}
  | maybeEvent == Nothing = m
  | otherwise = m {eventTree = uncurry insert (getMaybe maybeEvent) eventTree,
                    getState=View, getInputEvent = newInput}
  where
    maybeEvent = readEventInput getInputEvent
    getMaybe (Just v) = v
    getMaybe Nothing = (Date 0 0 0, AllDay "")

-------------------------------------------------------------------------------
-- Input Handling and Parsing

-- Handles input field updates
inputUpdateModel :: MisoString -> InputField -> Model -> Model
inputUpdateModel str field m@Model {..}
  = m {getInputEvent = updateInput str field getInputEvent}

-- Depending on event type, converts input to date and event
readEventInput :: InputEvent -> Maybe (Date, Event)
readEventInput ie@InputEvent {getEventType = et, getMer1=mer1, getMer2=mer2}
  | et == NewAllDay = (\(_, name, xs) ->
    (Date (head xs) (xs!!1) (xs!!2), AllDay name)) <$> parsed
  | et == NewTask = (\(_, name, xs) ->
    (Date (head xs) (xs!!1) (xs!!2),
      Task name (Time (xs!!3 + readMer mer1) (xs!! 4)))) <$> parsed
  | et == NewEvent = (\(_, name, xs) ->
    (Date (head xs) (xs!!1) (xs!!2),
      Event name (Time (xs!!3 + readMer mer1) (xs!! 4)) (Time (xs!!5 + readMer mer2) (xs!!6)))) <$> parsed
  | otherwise = Nothing
  where
    parsed = parseInputs ie
    readMer AM = 0
    readMer PM = 12

-- Converts input to a tuple containing pertinant information for event
parseInputs :: InputEvent -> Maybe (EventType, String, [Int])
parseInputs InputEvent {..} = readInputs (getEventType, getInputName, vals getEventType)
  where
    vals NewAllDay  = [getInputMonth, getInputDay, getInputYear]
    vals NewTask    = vals NewAllDay ++ [getHour1, getMin1]
    vals NewEvent   = vals NewTask ++ [getHour2, getMin2]
    vals NoEvent    = []

-- Parses throguh Miso Strings to their respective values
readInputs :: (EventType, MisoString, [MisoString]) -> Maybe (EventType, String, [Int])
readInputs (e, name, vals)
  | valDate == Nothing = Nothing
  | otherwise = fmap (\ls -> (e, fromMisoString name :: String, ls)) parsedVals
  where
    parsedVals = mapM (eitherToMaybe . fmse) vals :: Maybe [Int]
    valDate = parsedVals >>= (\xs -> checkValidDate (head xs) (xs!!1) (xs!!2))
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right v) = Just v

-- Wrapper for fromMisoStringEither to convert empty cases to 0
fmse :: MisoString -> Either String Int
fmse str
  | str == ms "" = Right 0
  | otherwise = fromMisoStringEither str
