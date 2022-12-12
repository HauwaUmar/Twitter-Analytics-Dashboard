# Twitter-Analytics-Dashboard
This dashboard is final project submission for DATA 6200 at the Univeristy of Guelph. The dashboard’s overall goal is to provide insight into a company’s presence on Twitter as
well as insight into a potential influencer for the brand. The dashboard attempts to do the following with the help of Sentiment Analysis.

The data used in this dashbaord is dynamic and is dependant on users input. It is made available with the help of rtweet which allows for easy connection to twitter API for R users. The package was created by Michael W. Kearney. For the purpose of this dashobaord, tweets will be extrcated based on two inputs:
* Keyword/Product/Hashtag
* Twitter Account

# Description
The dashboard is created in R using Rshiny.

### Prerequisites
To run this dashboard it is advisable to have the latest version of Rstudio and R.

There are packages needed to successfully run this dashboard:
 ```sh
   install.packages(c('rtweet','shiny','shinydashboard','sentimentr','wordcloud','ggplot2','plotly','tm','reactable','magrittr','stringr','lubridate','dplyr','glue','purrr'))
   ```

### Authenticate rtweet
To be able to extract tweets for free using r tweet, below is the authentication step to configure and confirm if it was done successfully.
 ```sh
 library (rtweet)
  # rtweet authentication
   auth_setup_default()

  # check if authentication exists
   auth_has_default()
   ```
  Potential users of the package should have a twitter account and login after running the first line of code .

# Running dashboard
After installing all the dependencies and authenticating your twitter account run the following line of code on R.

 ```sh
library(shiny)
runGitHub("Twitter-Analytics-Dashboard", "HauwaUmar")
 ```
