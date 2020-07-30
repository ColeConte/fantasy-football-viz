# fantasy-football-viz
Fantasy football metrics and visualization project using R Shiny. 

# API Connection
The first step of this project was to connect to the Yahoo and ESPN APIs. Since we're focused on player analytics, connecting to Yahoo's API ended up being a bit of a bust, since they don't give out player projection data (it's through a third party). ESPN's API is undocumented, but through some research it's accessible and does include player data. I added private league support by including two login-related cookies in the GET calls to the API.

For privacy's sake, I didn't include the files with my league IDs, Yahoo developer credentials, or ESPN cookies, but they're simple text files with one ID/credential/cookie per line.

Here's a short video I made on how to create a Yahoo Developer App without a redirect URI:

[![Create A Yahoo Developer App Without A Redirect URI](YahooDevScreenshot.png)](https://www.youtube.com/watch?v=RDMRtdP6XSc)
