module View where

import Miso
import qualified Miso.Html.Element
import Miso.String (ms)
import Numeric (showGFloat)

import DateTime
import Event
import Model
import Update

-- Creates HTML for the page
viewModel :: Env -> Model -> View Action
viewModel env m =
  div_
    [styleInline_ . ms $ styleVar "time" (getNow m)]
    [ Miso.Html.Element.style_ [] (ms $ getCss env)
    , viewModal m
    , div_ [id_ $ ms "top"] [ h1_ [] [text $  (ms . calMonth . currDate) m]]
    , div_ [id_ $ ms "bottom"] [ div_ [id_ $ ms "sidebar"] [], if' (getMode m == Week) viewWeek viewMonth m]
    , div_ [id_ $ ms "tooSmall"] [ h1_ [] [text $ ms "Your screen is currently too small for this app."]]
    ]

-------------------------------------------------------------------------------
-- View New Event Modal

-- Creates HTML for the New Event Modal
viewModal :: Model -> View Action
viewModal m =
  div_ (if' (getState m == Edit) [id_ $ ms "modal"] [id_ $ ms "modal", styleInline_ . ms $ "display: none;"])
    [ div_ [id_ $ ms "modalBackground"] []
    , div_ [id_ $ ms "modalDiv"]
      [ div_ [id_ $ ms "modalTop"]
        [ h4_ [] [(text . ms) "New Event"]
        , button_ [exitHandler] [span_ [] [(text . ms) "Cancel"]]]
      , div_ [id_ $ ms "modalMain"]
        (div_ [id_ $ ms "buttons"]
          [ button_ (handler NewAllDay) [(text . ms) "All Day"]
          , button_ (handler NewTask) [(text . ms) "Task"]
          , button_ (handler NewEvent) [(text . ms) "Event"]
          ] : modalMain)
      ]
    ]
  where
    exitHandler = on (ms "click") emptyDecoder $ const ViewUpdate
    buttonHandler e = on (ms "click") emptyDecoder $ const (EventUpdate e)
    handler e = if' (eventType == e) [(class_ . ms) "active", buttonHandler e] [buttonHandler e]
    doneHandler = on (ms "click") emptyDecoder $ const AddEventUpdate
    doneButton = [button_ [(id_ . ms) "done", doneHandler] [(text . ms) "Done!"]]
    eventType = (getEventType . getInputEvent) m
    mers = ((getMer1 . getInputEvent) m, (getMer2 . getInputEvent) m)
    modalMain = if' (eventType == NoEvent) [] (viewModalMain eventType mers ++ doneButton)
    
-- Creates HTML for main body of the New Event Modal
viewModalMain :: EventType -> (Meridiem, Meridiem) -> [View Action]
viewModalMain NewAllDay _ =
    [ div_ [(id_ . ms) "nameInput"] [ span_ [] [(text . ms) "Event Name"], input N0]
    , div_ [(id_ . ms) "dateInput"]
      [ div_ [] [span_ [] [(text . ms) "Month"], input M0]
      , div_ [] [span_ [] [(text . ms) "Day"], input D0]
      , div_ [] [span_ [] [(text . ms) "Year"], input Y0]]
    ]
viewModalMain NewTask (mer1, mer2) =
  viewModalMain NewAllDay (mer1, mer2) ++
  [ div_ [(id_ . ms) "timeInput"]
      [ div_ []
        [ div_ [] [span_ [] [(text . ms) "Hour"], input H1]
        , div_ [] [span_ [] [(text . ms) "Minute"], input M1]
        , div_ [class_ $ ms "mButtons"] [button 1 mer1 AM, button 1 mer1 PM]]
      ]
  ]
  where 
    merHandler i mer = on (ms "click") emptyDecoder $ const (MeridiemUpdate i mer)
    button i mer AM = button_ (if' (mer == AM) [merHandler i AM, class_ $ ms "active"] [merHandler i AM]) [text . ms $ "am"]
    button i mer PM = button_ (if' (mer == PM) [merHandler i PM, class_ $ ms "active"] [merHandler i PM]) [text . ms $ "pm"]
viewModalMain NewEvent (mer1, mer2) =
  viewModalMain NewAllDay (mer1, mer2) ++
  [ div_ [(id_ . ms) "timeInput"]
      [ div_ []
        [ div_ [] [span_ [] [(text . ms) "Hour"], input H1]
        , div_ [] [span_ [] [(text . ms) "Minute"], input M1]
        , div_ [class_ $ ms "mButtons"] [button 1 mer1 AM, button 1 mer1 PM]]
      , div_ []
        [ div_ [] [span_ [] [(text . ms) "Hour"], input H2]
        , div_ [] [span_ [] [(text . ms) "Minute"], input M2]
        , div_ [class_ $ ms "mButtons"] [button 2 mer2 AM, button 2 mer2 PM]]
      ]
  ]
  where 
    merHandler i mer = on (ms "click") emptyDecoder $ const (MeridiemUpdate i mer)
    button i mer AM = button_ (if' (mer == AM) [merHandler i AM, class_ $ ms "active"] [merHandler i AM]) [text . ms $ "am"]
    button i mer PM = button_ (if' (mer == PM) [merHandler i PM, class_ $ ms "active"] [merHandler i PM]) [text . ms $ "pm"]
viewModalMain _ _ = []

-- Creates input element given a input field
input :: InputField -> View Action
input N0 = input_ [inputHandler N0]
input Y0 = input_ [(maxlength_ . ms) "4", (pattern_ . ms) "[0-9]{0,4}", inputHandler Y0]
input field = input_ [(maxlength_ . ms) "2", (pattern_ . ms) "[0-9]{0,4}", inputHandler field]

-- creates an input handler given an input field
inputHandler :: InputField -> Attribute Action
inputHandler field = on (ms "input") valueDecoder $ \str -> InputUpdate str field

-------------------------------------------------------------------------------
-- View Main Calendar

-- Creates HTML for a weekly calendar
viewWeek :: Model -> View Action
viewWeek m@Model {getToday=t, currDate=date, eventTree=et} =
  div_ [id_ $ ms "week"]
    [ div_ [id_ $ ms "dates"]
            (div_ [id_ $ ms "timeline-top"] [ button_ [clickHandler] [span_ [] [(text . ms) "+"]]]
            : map (\(Agenda day _) ->
              div_ (if' (day == t) [class_ $ ms "today"] [])
                [h4_ []
                  [(text . ms . show . getDay) day]
                  , span_ [] [(text . ms . weekToStr . weekday) day]
                ]) week)
    , div_ [id_ $ ms "days"]
      [ div_ [id_ $ ms "alldays"]
        (div_ [id_ $ ms "timeline-top"] []
        : map (\(Agenda _ e) -> div_ [] (map viewEvent (getAllDay e))) week)
      , div_ [id_ $ ms "agenda"]
          [ div_ [] (div_ [id_ $ ms "timeline-bottom"] (viewTimeline m)
          : map (\(Agenda _ e) -> div_ [] (map viewEvent (getNotAllDay e))) week)
          ]
      ]
    ]
  where
    week = map (`getAgenda` et) $ genWeek date
    clickHandler = on (ms "click") emptyDecoder $ const ViewUpdate

-- Creates HTML the timeline on left for the Weekly View
viewTimeline :: Model -> [View Action]
viewTimeline m =
    if' ((genWeek . getToday) m /= (genWeek . currDate) m)
      [] [div_ [id_ $ ms "line"] [], div_ [id_ $ ms "point"] []] ++
    map (\hr -> div_ [] [span_ [] [text $ ms hr]]) hours
    where
        hours = map toAM [(1 :: Int) .. 11] ++ map toPM ((12 :: Int) : [1 .. 11]) ++ [""]
        toAM hr = show hr ++ " am"
        toPM hr = show hr ++ " pm"

-- Creates HTML for a monthly calendar
viewMonth :: Model -> View Action
viewMonth m =
  div_ [id_ $ ms "month"]
    ( map (\str -> div_ [] [(text . ms) str]) weekdays ++
      map (\(Agenda day e) -> div_ (monthClass m day) (span_ [] [(text . ms . getDay) day] : map viewEvent e)) month
    )
  where month = map (`getAgenda` eventTree m) $ (genMonth . currDate) m

-- Creates HTML for an individual event
viewEvent :: Event -> View Action
viewEvent (AllDay name) = div_ [class_ $ ms "allday"] [span_ [] [text $ ms name]]
viewEvent (Task name t) = div_ [class_ $ ms "task", styleInline_ . ms $ styleVar "start" t] [span_ [] [text $ ms name]]
viewEvent (Event name t1 t2) = div_ [class_ $ ms "event", styleInline_ . ms $ (styleVar "start" t1 ++ styleVar "length" (eventLength t1 t2))] [span_ [] [text $ ms name]]

-- Creates CSS percentage variables given a time
styleVar :: String -> Time -> String
styleVar pre t = "--" ++ pre ++ ":" ++ (round2 . timePercent) t ++ "%;"
  where 
    timePercent (Time hr minute) = (100 *) $ (fromIntegral hr / 24) + (fromIntegral minute / (60 * 24))
    round2 f = showGFloat (Just 2) (f :: Float) ""

-- Generates the corresponding classes for each date
monthClass :: Model -> Date -> [Attribute Action]
monthClass m d
    | d == getToday m = [(class_ . ms) "today"]
    | getMonth d /= (getMonth . currDate) m = [(class_ . ms) "notNow"]
    | otherwise = []