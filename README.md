# fantasy-football-viz
Fantasy football metrics and visualization project using R Shiny. 

# API Connection
The first step of this project was to connect to the Yahoo and ESPN APIs. Since we're focused on player analytics, connecting to Yahoo's API ended up being a bit of a bust, since they don't give out player projection data (it's through a third party). ESPN's API is undocumented, but the endpoints don't require authentication to access, and we can get player data.
