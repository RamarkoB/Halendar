# Halendar: A Haskell Calendar

Start the app by running ‘cabal run’ in the development directory. This uses `Main.hs` to kick start the application.

There are three core components to the code:
- The Pure Part: I used a binary tree to store events. I created Agendas, which consisted of a date and its corresponding events. The tree was then sorted by the dates, making retrieving and storing the events easy. There were 3 types of events: All Day Events, Tasks, and Standard Events. This can be found in `DateTime.hs`, which contains all the logic for Dates and time, such as getting the number of days in a month or changing Dates, and `Event.hs`, which contains all the logic for storing and getting events.

- The Viewing Part: The model was broken into individual HTML components. All stylization was done through the attached CSS file within the folder. It used the “today” and `rightNow` variables to check the date and time and the `currDate` variable to check the user’s selected date. Depending on the mode (Week or Month) and state (View or Edit), the page would represent the model in HTML by generating the needed elements from the pure functions in DateTime, and mapping them to HTML components. THis can be found in `View.hs` which contains all the logic for converting the Model into HTML.

- The Input Part:  Most of the input handling was done through HTML’s “pattern” and “maxLength” values, meaning that I knew that I would always receive a valid number or an empty string that could then be converted into an int using Miso’s MisoString converters. I built a new struct called inputEvent that would take in all the input fields and then could be converted into an event stored in the event tree. This can be found in `Model.hs`, which contains all the model definitions and InputEvents, and `Update.hs`, which contains all the logic for updating the model on different actions.

  
