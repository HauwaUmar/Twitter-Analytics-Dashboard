########################## Install Packages ##################
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("rtweet")
#install.packages("sentimentr")
#install.packages("wordcloud")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("magrittr")
#install.packages("reactable")
#install.packages("tm")
###############################################################





###### Importing Relevant Libraries #########
library(shiny)
library(shinydashboard)
library(rtweet)

# text/data manipulation
library(dplyr)
library(lubridate)
library(magrittr)
library(stringr)
library(purrr)
library(glue)


# for table
library(reactable)

# for sentiment analysis
library(sentimentr)

# for making plots
library(ggplot2)
library(plotly)
library(wordcloud)
library(tm)

# rtweet authentication
auth_setup_default()

# check if authentication exists
auth_has_default()

############## this function cleans text for sentiment analysis ####################
clean_text = function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = sub("([.-])|[[:punct:]]", "\\1",x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http[^[:space:]]*", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", " ", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

########## function to clean column for device used to tweet ######### 
get_source <- function(x){
  x = gsub(".*rel=\"nofollow\">", "", x )
  x  = gsub("</a.*", "", x)
  return(x)
}


###### tokenize tweets for word cloud #####
plot_wordcloud <- function(text){
  text_corpus <- Corpus(VectorSource(text))
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords("english")))
  set.seed(123)
  return(text_corpus)
}


#Create a ui (user interface); commands that create the overall look of the dashboard
ui <- dashboardPage(
  skin="blue", #Set colour of the dashboard, 
  dashboardHeader(title = "Twitter Analytics"), #Title dashboard
  dashboardSidebar( # UI for the Sidebar
    sidebarMenu(id="menu1", #Creates a menu of tab names 
                menuItem("Product Analysis", tabName="dashboard", icon=icon("twitter")),
                menuItem("User Timeline Analysis", tabName="analysis", icon=icon("chart-line")),
                menuItem("About", tabName="about", icon=icon("info"))
    )),
  dashboardBody( # UI of the mainPanel of each tab
    tabItems( # One tabItem() for each of the three tabs
      #FIRST TAB: Product Analysis
      tabItem(tabName="dashboard",
              div(
                style='text-align:center;',
                h2("Product Analysis")
              ),
              #First row on the first tab
              fluidRow(
                box(status="primary",title='Dates extracted',solidHeader = TRUE,width=3,
                    collapsible = TRUE,collapsed = TRUE,
                    div(style="float:left;padding:10px;",
                        h4(textOutput("mindate")),
                        h4(textOutput("maxdate")))),
                # code for the search button
                div(style = "float:right;padding:10px", 
                    actionButton("reset", "Search",icon=icon("plus"), color = "warning",
                                 style="display: inline-block;
                    border-radius: 4px;
                    background-color: #ff8600;
                    border: none;
                    color: #FFFFFF;
                    text-align: center;
                    font-size: 15px;
                    padding: 10px;
                    width: 80px;
                    transition: all 0.5s;
                    cursor: pointer;
                    margin: 5px;
                            ")
                )
              ),
              #Second row on the first tab
              fluidRow(
                #info box  containing word entered by users
                infoBox(
                  width=3,
                  "Keyword Search",uiOutput("keyword"),icon=icon("hashtag"),color = "orange"
                ),
                #info box containing number of tweets extracted 
                infoBox(width = 3,
                        "Number of Tweet", uiOutput("num"), icon = icon("twitter"),color = "light-blue"
                ),
                #info box containing number of users that contributed to the tweets 
                infoBox(width = 3,
                        "Number of Users",  uiOutput("num_of_user"), icon = icon("users"), color = "orange"
                ),
                #info box containing number of engagement
                infoBox(width = 3,
                        "Number of Engagements", uiOutput("num_of_likes"), icon = icon("thumbs-up"), color = "light-blue"
                        
                )
                
              ),
              #Second row on the first tab
              fluidRow(
                box(status = "primary",collapsible = TRUE,width = 12,
                    tabsetPanel(type = "tabs", # creates tab panel for this row
                                tabPanel("Frequency of tweets", # sets tab panel to show frequency of tweets
                                         selectInput(#adds dropdown to show plot on a daily basis or hourly basis
                                           "variable", "Variable:",
                                           c("Daily" = "created_at_round_day",
                                             "Hourly" = "created_at_round"))
                                         ,
                                         plotlyOutput("regfreq") %>% withSpinner(color="#ff8600"),
                                         height = "400px",
                                         width = "400px"), 
                                tabPanel("Frequency of tweets by Sentiment", # sets tab panel to show frequency of tweets with respect to sentiment
                                         selectInput("variable_2", "Variable:",
                                                     c("Daily" = "created_at_round_day_2",
                                                       "Hourly" = "created_at_round_2")),
                                         plotlyOutput("freqposts") %>% withSpinner(color="#ff8600"))
                    )
                )),
              #Third row on the first tab
              fluidRow(
                box(title = "Source used to tweet",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width=12,height=565,
                    selectInput("variable_3", "Variable:",
                                c("Without Sentiment" = "no_senti",#adds dropdown to show plot on with sentiment or without sentiment
                                  "With Sentiment" = "senti")),
                    plotlyOutput("source") %>% withSpinner(color="#ff8600")
                ))
              ,
              br(),
              # fourth row on the first tab
              fluidRow(
                box(title = "Word Cloud",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width=12,
                    #adds side panel with two slider 
                    sidebarPanel(
                      sliderInput("freq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15),
                      sliderInput("max",
                                  "Maximum Number of Words:",
                                  min = 1,  max = 300,  value = 100)
                    ),
                    mainPanel(
                      plotOutput("wordcloud") %>% withSpinner(color="#ff8600")#adds word cloud to box
                    )
                )
              ),
              br(),
              # fifth row the first tab
              fluidRow(
                box(width=12,
                    title = "Table of tweets",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    selectInput("variable_4", "Variable:",
                                c("All Tweets" = "all",
                                  "Positive Tweets" = "pos",
                                  "Negative Tweets" = "neg",
                                  "Neutral Tweets" = "neu")),
                    reactableOutput("tweet_fav_positive_table")
                )
              )
      )
      
      ,
      #SECOND TAB: User Timeline Analysis
      tabItem(tabName="analysis",
              div(
                style='text-align:center;',
                h2("User Timeline Analysis")
              ),
              fluidRow(
                box(status="primary",title='Dates extracted',solidHeader = TRUE,width=3,
                    collapsible = TRUE,collapsed = TRUE,
                    div(style="float:left;padding:10px;",
                        h4(textOutput("usermindate")),
                        h4(textOutput("usermaxdate")))),
                div(style = "float:right;padding:10px", 
                    actionButton("user_submit", "Search",icon=icon("plus"), color = "warning",style="display: inline-block;
                    border-radius: 4px;
                    background-color: #ff8600;
                    border: none;
                    color: #FFFFFF;
                    text-align: center;
                    font-size: 15px;
                    padding: 10px;
                    width: 80px;
                    transition: all 0.5s;
                    cursor: pointer;
                    margin: 5px;")
                )
              ),
              fluidRow(
                infoBox(
                  width=3,
                  "Screen Name",uiOutput("username"),icon = icon("hashtag"),color = "orange"
                ),
                infoBox(
                  width=3,
                  "Number of followers",uiOutput("num_of_followers"),icon = icon("users"),color = "light-blue"
                ),
                infoBox(
                  width=3,
                  "Average Engagement Rate",uiOutput("average_eng_rate"),icon = icon("calendar-day"),color = "orange"
                ),
                infoBox(
                  width=3,
                  "Number of Tweets",uiOutput("user_tweet_count"),icon = icon("calendar-day"),color = "light-blue"
                )
                
              ),
              fluidRow(
                box(width = 12,
                    status="primary",collapsible = TRUE,
                    tabsetPanel(
                      tabPanel("Number of Tweets",
                               selectInput("variable_7", "Variable:",
                                           c("Daily" = "created_at_round_day",
                                             "Hour" = "created_at_round")),
                               plotlyOutput("user_freq")%>% withSpinner(color="#ff8600")
                      ),
                      tabPanel("Average Engagement Rate",
                               selectInput("variable_8", "Variable:",
                                           c("Daily" = "created_at_round_day",
                                             "Hourly" = "created_at_round")),
                               plotlyOutput("plot_freq_engagement_rate")%>% withSpinner(color="#ff8600")
                      )
                    )
                )
              ),
              fluidRow(
                box(title="Analysis on User's Post", solidHeader = TRUE,
                    width = 6,
                    status="primary",collapsible = TRUE,
                    selectInput("plottype", "Variable:",
                                c("Bar Chart" = "bar",
                                  "Pie Chart" = "pie")),
                    plotlyOutput("user_senti_plot")%>% withSpinner(color="#ff8600"))
                
                ,
                box(width=6,
                    title = "Best Time to Post ",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    selectInput("variable_5", "Variable:",
                                c("Likes" = "likes",
                                  "Retweets" = "retweet",
                                  "Both" = "both")),
                    plotlyOutput("heat_map_post_optimization")%>% withSpinner(color="#ff8600")
                )
              ),
              br(),
              fluidRow(
                box(title = "Word Cloud",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    width=12,
                    sidebarPanel(
                      sliderInput("userfreq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15),
                      sliderInput("usermax",
                                  "Maximum Number of Words:",
                                  min = 1,  max = 300,  value = 100)
                    ),
                    mainPanel(
                      plotOutput("userwordcloud") %>% withSpinner(color="#0dc5c1")
                    )
                )
              ),
              
              br(),
              fluidRow(
                box(title = "Table of tweets", width=12,
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    tabsetPanel(
                      tabPanel(id = "all_user_table",'Positive Tweets',
                               selectInput("variable_9", "Variable:",
                                           c("Likes" = "likes",
                                             "Retweets" = "retweet",
                                             "Both" = "both")),
                               reactableOutput("user_positive_table")
                      ),
                      tabPanel(id = "pos_user_table",'Negative Tweets',
                               selectInput("variable_10", "Variable:",
                                           c("Likes" = "likes",
                                             "Retweets" = "retweet",
                                             "Both" = "both")),
                               reactableOutput("user_negative_table")
                      ),
                      tabPanel("All Tweets",
                               selectInput("variable_11", "Variable:",
                                           c("Likes" = "likes",
                                             "Retweets" = "retweet",
                                             "Both" = "both")),
                               reactableOutput("user_table")
                      )
                    )
                    
                )
              )
              
      ),
      #THIRD TAB: About
      tabItem(
        tabName = 'about',
        div(
          style='text-align:center;',
          h1('FUNCTIONALITIES OF DASHBOARD')
        ),
        fluidRow(
          box(width=4,status='info',
              h1("Monitor & analyze your brand on Twitter"),
              p("Track & analyze various Twitter metrics to get valuable insights about your Twitter presence. 
          Simply enter keyword or hashtag in the Product Analysis section and collect important mentions daily."),
              tags$ul(
                tags$li("Receive insight on how frequently people are talking about your hashtag or campaign on an hourly or daily basis"),
                tags$li("Receive insight on the sentiment behind each post i.e. negative, positive, or neutral using sentiment analysis (sentiment r)"),
                tags$li("Know the device used to make tweets about your hashtag or campaign (iPhone, android, e.t.c.)"),
                tags$li("Receive insight into frequently used words when talking about your brand or campaign"),
                tags$li("View the positive, negative or neutral tweets determined by sentiment analysis library")
              )),
          box(width=4,status='info',
              h1("Analyze Company's Twitter Account"),
              p("Analyze potential Influencers engagement metrics get valuable insight about their twitter presence.
              Simply enter their username on twitter in the User Timeline Analysis section and get the following insight on their page:"),
              tags$ul(
                tags$li("Receive insight into how frequently an influencer posts on their account on a daily or monthly basis."),
                tags$li("Receive insight on the sentiment behind each post i.e. negative, positive, or neutral using sentiment analysis (sentiment r)."),
                tags$li("Know what day and time of the week an account gets the most engagement."),
                tags$li("Receive insight into frequently used words by the influencer."),
                tags$li("View the positive, negative or neutral tweets determined by sentiment analysis library and ordered by top likes, retweets or both")
              )),
          box(width=4,status='info',
              h1("Recruit potential Influencers"),
              p("Analyze potential Influencers engagement metrics get valuable insight about their twitter presence.
              Simply enter their username on twitter in the User Timeline Analysis section and get the following insight on their page:"),
              tags$ul(
                tags$li("Receive insight into how frequently an influencer posts on their account on a daily or monthly basis."),
                tags$li("Receive insight on the sentiment behind each post i.e. negative, positive, or neutral using sentiment analysis (sentiment r)."),
                tags$li("Know what day and time of the week an account gets the most engagement."),
                tags$li("Receive insight into frequently used words by the influencer."),
                tags$li("View the positive, negative or neutral tweets determined by sentiment analysis library and ordered by top likes, retweets or both")
              )
          )
          
        ),
        fluidRow(
          box(
            title = "About this Dashboard",
            status = "primary",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
            width = 6,
            #div(style = "text-align:center;", 
            #h1("About this dashboard")),
            tags$p(
              class = "text-center",
              tags$a(
                href = "https://www.r-project.org",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "https://www.r-project.org/logo/Rlogo.svg",
                         style = "max-width: 150px;"
                )
              ),
              tags$a(
                href = "https://rstudio.com",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "RStudio.svg",
                         style = "max-width: 150px; margin-left: 2em;"
                )
              ),
              tags$a(
                href = "https://rtweet.info",
                target = "_blank",
                tags$img(class = "image-responsive",
                         src = "rtweet.png",
                         style = "max-width: 150px; margin-left: 2em;"
                )
              )
              
            ),
            tags$p(
              "This dashboard was built in",
              tags$a(href = "https://r-project.org", target = "_blank", "R"),
              "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
              tags$strong("shiny,"),
              tags$strong("shinydashboard,"),
              tags$strong("rtweet,"),
              tags$strong("plotly,"),
              "the", tags$strong("tidyverse,"),
              "and many more packages."
            )
            
          ),
          box(title = "Dashboard Developer",
              status = "primary",solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
              width = 6,
              h4('Name of Developer:  Hauwa Umar'),
              br(),
              h4('Email:  umarhauwa67@gmail.com'),
              br(),
              h4('Github:',tags$a(href = "https://github.com/HauwaUmar", target = "_blank", "@HauwaUmar"))
          )
        )
        
      )
      
    )
  )
)






# Define server logic 
server <- function(input, output) {
  
  ###############################################################################################################################
  #FOR PRODUCT ANALYSIS
  ###############################################################################################################################  
  
  # pops up modal box for users to enter keyword
  l <- reactiveValues()
  observeEvent(input$reset, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      tags$h2('Hashtag to search'),
      textInput('name', 'Tweet to Search'),
      numericInput('state', 'Number',value=100),
      footer=tagList(
        actionButton('submit', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  # only store the information if the user clicks submit
  observeEvent(input$submit, {
    removeModal()
    l$name <- input$name
    l$state <- input$state
  })
  
  # retrieving tweets based on keyword and number
  tweet_df <- eventReactive(input$submit, {
    search_tweets(l$name, n = l$state, include_rts = FALSE,lang="en",type = "recent")
  })
  
  # retrieving relevant columns for display
  tweet_table_data <- reactive({
    req(tweet_df())
    tweet_df() %>%
      select(created_at, text, favorite_count, retweet_count) %>%
      mutate(
        Tweet =text,
        URLs = "",
        created_at = created_at %>% 
          # Remove zeros.
          str_remove_all(pattern = '\\+0000') %>%
          # Parse date.
          parse_date_time(orders = '%y-%m-%d %H%M%S')
      )%>%
      select(DateTime = created_at, Tweet, Likes = favorite_count, RTs = retweet_count)
  })
  
  tweet_clean_text <- reactive ({
    req(tweet_df())
    tweet_df()%>%mutate(clean_text = text%>%clean_text(),
                        source = source %>% get_source(),
                        created_at_round = created_at%>% round(units = 'hours') %>% as.POSIXct(),
                        created_at_round_day = as.Date(created_at))
  })
  
  
  tweet_sentiment_score <-  reactive({
    req(tweet_clean_text())
    tweet_clean_text() %>%  
      mutate(sentiment_score = (clean_text%>%get_sentences()%>%sentiment_by())[,4])                                 
    
  })
  
  
  tweet_sentiment_tag <- reactive({
    req(tweet_sentiment_score())  
    tweet_sentiment_score()%>% mutate(sentiment_tag =
                                        case_when(sentiment_score <= 0.2 & sentiment_score >= -0.2  ~ "neutral", 
                                                  sentiment_score < -0.2  ~ "negative",
                                                  sentiment_score > 0.2 ~ "positive"))
  })
  
  tweet_source_tag<- reactive({
    req(tweet_sentiment_tag()) 
    tweet_sentiment_tag() %>% mutate(source =
                                       case_when(source == "Twitter for iPhone" ~ "iPhone", 
                                                 source == "Twitter Web App" ~ "Web App",
                                                 source == "Twitter for Android" ~ "Android",
                                                 TRUE ~ "Other")
    )
  })
  
  # gets positive tweets fro product analysis
  tweet_fav_positive_table <- reactive({
    req(tweet_source_tag())
    if (input$variable_4 == "all"){
      tweet_source_tag()[order(-tweet_source_tag()$favorite_count),] %>%
        dplyr::select(created_at, text, favorite_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        dplyr::select(DateTime = created_at, Tweet, Likes = favorite_count)
    }
    else if (input$variable_4 == "pos"){
      tweet_source_tag()[order(-tweet_source_tag()$favorite_count),] %>%
        filter(sentiment_tag=="positive") %>%
        dplyr::select(created_at, text, favorite_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        dplyr::select(DateTime = created_at, Tweet, Likes = favorite_count)
    }
    else if (input$variable_4 == "neg"){
      tweet_source_tag()[order(-tweet_source_tag()$favorite_count),] %>%
        filter(sentiment_tag=="negative") %>%
        dplyr::select(created_at, text, favorite_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Likes = favorite_count)
    }
    else {
      tweet_source_tag()[order(-tweet_source_tag()$favorite_count),] %>%
        filter(sentiment_tag=="neutral") %>%
        select(created_at, text, favorite_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Likes = favorite_count)
    }
  })
  
  #gets the earliest date of tweet retrieved
  min_date <- reactive({
    req(tweet_df())
    (tweet_df()%>%summarise(min(as.Date(created_at))))[[1]]
  })
  
  #gets the latest date of tweet retrieved
  max_date <- reactive({
    req(tweet_df())
    (tweet_df()%>%summarise(max(as.Date(created_at))))[[1]]
  })
  
  # groups tweets by day or hour of the day
  tweet_freq <- reactive({
    req(tweet_df())
    if (input$variable == "created_at_round_day"){
      tweet_source_tag()%>% group_by(created_at_round_day) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
    }
    else {
      tweet_source_tag()%>% group_by(created_at_round) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
    }
    
  })
  
  #plots number of tweets extracted on a daily/hourly basis 
  plot_freq <- reactive({
    req(tweet_freq())
    if (input$variable == "created_at_round_day"){#daily basis
      tweet_freq()%>%ggplot(aes(x =created_at_round_day,y = total_count))+
        geom_line(color="#0077b6")+
        geom_point(color="#ff8600")+
        xlab(label = 'Date') +
        ylab(label = 'Number of Tweets')+scale_fill_brewer()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else {# hourly basis
      tweet_freq()%>%ggplot(aes(x =created_at_round,y = total_count))+
        geom_line(color="#0077b6")+
        geom_point(color="#ff8600")+
        xlab(label = 'Date') +
        ylab(label = 'Number of Tweets')+scale_fill_brewer()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    
  })
  
  
  plotsource <- reactive({
    req(tweet_source_tag())
    if (input$variable_3=="senti"){
      tweet_source_tag() %>%
        ggplot(mapping = aes(x = source,fill=sentiment_tag)) +
        theme_light() +
        geom_bar()+
        coord_flip()+
        xlab(label = 'Source') +
        ylab(label = NULL) +
        ggtitle(label = 'Source of tweets')+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else {
      tweet_source_tag() %>%
        ggplot(mapping = aes(x = source)) +
        theme_light() +
        geom_bar(fill="#0077b6")+
        coord_flip()+
        xlab(label = 'Source') +
        ylab(label = NULL) +
        ggtitle(label = 'Source of tweets')+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
  })
  
  # groups tweets by sentiment and date
  tweet_senti_group <- reactive({
    req(tweet_source_tag())
    if (input$variable_2 == "created_at_round_day_2"){ # daily basis
      tweet_source_tag()%>% group_by(created_at_round_day,sentiment_tag) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
    }
    else{ # hourly basis
      tweet_source_tag()%>% group_by(created_at_round,sentiment_tag) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
    }
    
  })
  
  #plots number of tweets over date 
  plotfreqsentiment <- reactive({
    req(tweet_senti_group())
    if (input$variable_2 == "created_at_round_day_2"){ # daily basis
      tweet_senti_group()%>%ggplot(aes(x = created_at_round_day,y = total_count,group=sentiment_tag,color=sentiment_tag))+
        geom_line()+
        geom_point()+
        xlab(label = 'Date') +
        ylab(label = 'Number of Tweets')+
        labs(color = "Sentiment")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
    }
    else { # hourly basis
      tweet_senti_group()%>%ggplot(aes(x = created_at_round,y = total_count,group=sentiment_tag,color=sentiment_tag))+
        geom_line()+
        geom_point()+
        xlab(label = 'Date') +
        ylab(label = 'Number of Tweets')+
        labs(color = "Sentiment")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    
  })
  
  # clean text for word cloud
  clean_text_source <- reactive({
    req(tweet_source_tag())
    tweet_source_tag()%>% dplyr::select(clean_text)
  })
  
  # takes clean text and passes makes it corpus
  text_corpus <- reactive({
    plot_wordcloud(clean_text_source())
  })
  
  # Total number of tweets extracted
  tweet_count <- reactive({
    req(tweet_df())
    tweet_df()%>%nrow()
  })
  
  # Total number of likes
  tweet_likes_count <- reactive({
    req(tweet_df())
    (tweet_df()%>%summarise(sum(favorite_count)))[[1]]
  })
  
  # Total number of retweets 
  tweet_retweet_count <- reactive({
    req(tweet_df())
    (tweet_df()%>%summarise(sum(retweet_count)))[[1]]
  })
  
  # number of users that tweeted 
  tweet_user_count <- reactive({
    req(tweet_df())
    (tweet_df()%>%users_data()%>%unique()%>%nrow())[[1]]
    
  })
  
  # plots a bar chart showing the device used to make tweets
  plotsource <- reactive({
    req(tweet_source_tag())
    if (input$variable_3=="senti"){# ploting with sentiment color coding
      tweet_source_tag() %>%
        ggplot(mapping = aes(x = source,fill=sentiment_tag)) +
        theme_light() +
        geom_bar()+
        coord_flip()+
        xlab(label = 'Source') +
        ylab(label = 'Frequency') +
        labs(color = "Sentiment") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else { # plotting without sentiment 
      tweet_source_tag() %>%
        ggplot(mapping = aes(x = source)) +
        theme_light() +
        geom_bar(fill="#ff8600")+
        coord_flip()+
        xlab(label = 'Source') +
        ylab(label = 'Frequency') +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
  })
  
  
  
  ###############################################################################################################################
  #FOR USER TIMELINE ANALYSIS
  ###############################################################################################################################
  # pops up modal box for users to enter twitter account username
  r <- reactiveValues()
  observeEvent(input$user_submit, {
    # display a modal dialog with a header, textinput and action buttons
    showModal(modalDialog(
      tags$h2('Username to search'),
      textInput('name_2', 'User to Search'),
      numericInput('num', 'Number',value=30),
      footer=tagList(
        actionButton('submit_submit', 'Submit'),
        modalButton('cancel')
      )
    ))
  })
  
  # only store the information if the user clicks submit
  observeEvent(input$submit_submit, {
    removeModal()
    r$name <- input$name_2
    r$num <- input$num
  })
  
  
  # retrieving tweets based on twitter username and number
  user_df <-eventReactive(input$submit_submit, {
    get_timeline(r$name, n = r$num)
  })
  
  
  # creating table with relevant information about twitter account tweets
  user_tweet_table <- reactive({
    req(user_df())
    user_df() %>%
      select(created_at, text, favorite_count, retweet_count) %>%
      mutate(
        Tweet =text,
        URLs = "",
        created_at = created_at %>% 
          # Remove zeros.
          str_remove_all(pattern = '\\+0000') %>%
          # Parse date.
          parse_date_time(orders = '%y-%m-%d %H%M%S')
      )%>%
      select(DateTime = created_at, Tweet, Likes = favorite_count, RTs = retweet_count)
  })
  
  #contains positive tweets from users account
  user_positive_table <- reactive({
    req(user_source_tag())
    if (input$variable_9 == 'likes'){
      user_source_tag()[order(-user_source_tag()$favorite_count),] %>%
        filter(sentiment_tag=='positive') %>%
        select(created_at, text, favorite_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Likes = favorite_count)
    }
    
    else if (input$variable_9 == 'retweet'){
      user_source_tag()[order(-user_source_tag()$retweet_count),] %>%
        filter(sentiment_tag=='positive') %>%
        select(created_at, text, retweet_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Retweets = retweet_count)
    }
    else {
      user_source_tag()[order(-user_source_tag()$engagement),] %>%
        filter(sentiment_tag=='positive') %>%
        select(created_at, text, engagement) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Engagement = engagement)
    }
  })
  
  #contains negative tweets from Twitter account
  user_negative_table <- reactive({
    req(user_source_tag())
    if (input$variable_10 == 'likes'){
      user_source_tag()[order(-user_source_tag()$favorite_count),] %>%
        filter(sentiment_tag=='negative') %>%
        select(created_at, text, favorite_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Likes = favorite_count)
    }
    
    else if (input$variable_10 == 'retweet'){
      user_source_tag()[order(-user_source_tag()$retweet_count),] %>%
        filter(sentiment_tag=='negative') %>%
        select(created_at, text, retweet_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Retweets = retweet_count)
    }
    else {
      user_source_tag()[order(-user_source_tag()$engagement),] %>%
        filter(sentiment_tag=='negative') %>%
        select(created_at, text, engagement) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Engagement = engagement)
    }
  })
  
  user_table <- reactive({
    req(user_source_tag())
    if (input$variable_11 == 'likes'){
      user_source_tag()[order(-user_source_tag()$favorite_count),] %>%
        select(created_at, text, favorite_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Likes = favorite_count)
    }
    
    else if (input$variable_11 == 'retweet'){
      user_source_tag()[order(-user_source_tag()$retweet_count),] %>%
        select(created_at, text, retweet_count) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Retweets = retweet_count)
    }
    else {
      user_source_tag()[order(-user_source_tag()$engagement),] %>%
        select(created_at, text, engagement) %>%
        mutate(
          Tweet =text,
          created_at = created_at %>% 
            # Remove zeros.
            str_remove_all(pattern = '\\+0000') %>%
            # Parse date.
            parse_date_time(orders = '%y-%m-%d %H%M%S')
        )%>%
        select(DateTime = created_at, Tweet, Engagement = engagement)
    }
  })
  
  
  # cleans tweets and rounds date on a daily and hourly basis
  user_clean_text <- reactive ({
    req(user_df())
    user_df()%>%mutate(clean_text = text%>%clean_text(),
                       source = source %>% get_source(),
                       created_at_round = created_at%>% round(units = 'hours') %>% as.POSIXct(),
                       created_at_round_day = as.Date(created_at))
  })
  
  
  
  
  
  # getting sentiment of each tweet
  user_sentiment_score <-  reactive({
    req(user_clean_text())
    user_clean_text() %>%  
      mutate(sentiment_score = (clean_text%>%get_sentences()%>%sentiment_by())[,4])                                 
    
  })
  
  
  
  
  # assigning tag to sentiment score
  user_sentiment_tag <- reactive({
    req(user_sentiment_score())  
    user_sentiment_score()%>% mutate(sentiment_tag =
                                       case_when(sentiment_score <= 0.2 & sentiment_score >= -0.2  ~ "neutral", 
                                                 sentiment_score < -0.2  ~ "negative",
                                                 sentiment_score > 0.2 ~ "positive"))
  })
  
  
  # adding the source column, engagement column, day column and hour column for further analysis
  user_source_tag<- reactive({
    req(user_sentiment_tag()) 
    user_sentiment_tag() %>% mutate(source =
                                      case_when(source == "Twitter for iPhone" ~ "iPhone", 
                                                source == "Twitter Web App" ~ "Web App",
                                                source == "Twitter for Android" ~ "Android",
                                                TRUE ~ "Other"),
                                    engagement = favorite_count+retweet_count,
                                    days=wday(created_at_round_day,label = TRUE,abbr=FALSE),
                                    hour = hour(created_at_round)
    )
  })
  
  
  #calculating engagement rate
  user_engagement_rate_df <- reactive({
    req(num_of_followers())
    user_source_tag() %>% mutate(
      engagement_rate = (engagement/num_of_followers())*100
    )
  })
  
  #grouping table by day or hour tweeted and finding average engagement rate
  user_created_day_by_engagement_rate <- reactive({
    req(user_engagement_rate_df())
    if (input$variable_8 == "created_at_round_day"){
      user_engagement_rate_df() %>% group_by(created_at_round_day)%>% 
        summarise(average_rate=mean(engagement_rate),.groups = 'drop')
    }
    else {
      user_engagement_rate_df() %>% group_by(created_at_round)%>% 
        summarise(average_rate=mean(engagement_rate),.groups = 'drop')
    }
  })
  
  # plotting line graph of average engagement rate on a daily or hourly basis
  plot_freq_engagement_rate <- reactive({
    req(user_created_day_by_engagement_rate())
    if (input$variable_8 == "created_at_round_day"){
      user_created_day_by_engagement_rate()%>%ggplot(aes(x =created_at_round_day,y = average_rate))+
        geom_line(color="#0077b6")+
        geom_point(color="#ff8600")+
        xlab(label = 'Date') +
        ylab(label = 'Average Engagement Rate')+scale_fill_brewer()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else {
      user_created_day_by_engagement_rate()%>%ggplot(aes(x =created_at_round,y = average_rate))+
        geom_line(color="#0077b6")+
        geom_point(color="#ff8600")+
        xlab(label = 'Date') +
        ylab(label = 'Average Engagement Rate')+scale_fill_brewer()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    
  })
  
  
  #grouping tweets by the day and hour posted and summing the total likes,retweets or engagements
  user_days_hour <- reactive({
    req(user_source_tag())
    if (input$variable_5 == "likes"){
      user_source_tag()%>% group_by(days,hour) %>% 
        summarise(Total=sum(favorite_count),.groups = 'drop')
    }
    else if (input$variable_5 == "retweet"){
      user_source_tag()%>% group_by(days,hour) %>% 
        summarise(Total=sum(retweet_count),.groups = 'drop')
    }
    else {
      user_source_tag()%>% group_by(days,hour) %>% 
        summarise(Total=sum(engagement),.groups = 'drop')
    }
    
  })
  
  
  # plotting heatmap based on day, hour and number of engagements
  heat_map_post_optimization <- reactive({
    req(user_days_hour())
    user_days_hour() %>% ggplot(aes(hour, days)) + 
      geom_tile(aes(fill= Total),color = "black",
                linewidth = 0.2,
                linetype = 1)+
      scale_fill_gradient(low="white", high="#ff8600") +
      #scale_fill_viridis(discrete=FALSE) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
  })
  
  #counts number of tweets extracted
  user_tweet_count <- reactive({
    req(user_df())
    user_df()%>%nrow()
  })  
  
  
  #gets number of followers of twitter account
  num_of_followers <- reactive({
    req(user_source_tag())
    (user_source_tag() %>% users_data() %>%unique() %>%  dplyr::select(followers_count))[[1]]
  })
  
  
  #gets screen name of twitter account
  screen_name <- reactive({
    req(user_source_tag())
    (user_source_tag()%>%users_data()%>%unique()%>% dplyr::select(name))[[1]]
  })
  
  #gets engagement rate of twitter account
  engagement_rate <- reactive({
    req(num_of_followers())
    (user_source_tag()$engagement / num_of_followers()) * 100
  })
  
  
  #gets average engagement rate of twitter account
  average_eng_rate<- reactive({
    req(engagement_rate)
    mean(unlist(engagement_rate()))
  })
  
  # gets minimum date 
  user_min_date <- reactive({
    req(user_df())
    (user_df()%>%summarise(min(as.Date(created_at))))[[1]]
  })
  
  # gets maximum date 
  user_max_date <- reactive({
    req(user_df())
    (user_df()%>%summarise(max(as.Date(created_at))))[[1]]
  })
  
  
  
  #groups number of tweets extracted on a daily / hourly basis
  user_freq_df <- reactive({
    req(user_source_tag())
    if (input$variable_7 == "created_at_round_day"){
      user_source_tag()%>% group_by(created_at_round_day) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
    }
    else {
      user_source_tag()%>% group_by(created_at_round) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
    }
    
  })
  
  #plots line graph number of tweets extracted vs date for Twitter account
  plot_user_freq <- reactive({
    req(user_freq_df())
    if (input$variable_7 == "created_at_round_day"){
      user_freq_df()%>%ggplot(aes(x =created_at_round_day,y = total_count))+
        geom_line(color="#0077b6")+
        geom_point(color="#ff8600")+
        xlab(label = 'Date') +
        ylab(label = 'Number of Tweets')+scale_fill_brewer()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else {
      user_freq_df()%>%ggplot(aes(x =created_at_round,y = total_count))+
        geom_line(color="#0077b6")+
        geom_point(color="#ff8600")+
        xlab(label = 'Date') +
        ylab(label = 'Number of Tweets')+scale_fill_brewer()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    
  })
  
  
  
  # cleans tweet for word cloud
  clean_user_text_source <- reactive({
    req(user_source_tag())
    user_source_tag()%>% dplyr::select(clean_text)
  })
  
  
  # creates corpus for word cloud
  user_text_corpus <- reactive({
    req(clean_user_text_source())
    plot_wordcloud(clean_user_text_source())
  })
  
  
  # count number of tweets bases on sentiment
  user_senti_group <- reactive({
    req(user_source_tag())
    user_source_tag()%>% group_by(sentiment_tag) %>% 
      summarise(total_count=n(),
                .groups = 'drop')
  })
  
  #plotting bar and pie chart of sentimemt group
  user_senti_plot <- reactive({
    req(user_senti_group())
    if (input$plottype == 'bar'){
      user_source_tag() %>%
        ggplot(mapping = aes(x = sentiment_tag,fill=sentiment_tag)) +
        theme_light() +
        geom_bar()+
        coord_flip()+
        xlab(label = 'Sentiment') +
        ylab(label = NULL) +
        labs(color = "Sentiment") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    
    
    
    else{
      user_senti_group() %>% plot_ly(type='pie', labels=~sentiment_tag, values=~total_count, 
                                     textinfo='label+percent',
                                     insidetextorientation='radial')
    }
  })
  
  
  
  
  ###############################################################################################################################
  #OUT PUT FOR DASHBOARD#
  ###############################################################################################################################
  output$source <- renderPlotly({
    plotsource()
  })
  
  
  
  output$freqposts <- renderPlotly({
    plotfreqsentiment()
  })
  output$user_plot_fav_scatter <- renderPlotly({
    input$submit_submit
    isolate(user_plot_fav_scatter())
  })
  
  output$regfreq <- renderPlotly({
    
    plot_freq()
  })
  
  output$user_freq <- renderPlotly({
    
    plot_user_freq()
  })
  
  
  
  output$wordcloud <- renderPlot({
    wordcloud(text_corpus(), min.freq = input$freq, max.words = input$max, scale = c(2.2,1),
              colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)
    
  })
  
  output$userwordcloud <- renderPlot({
    wordcloud(user_text_corpus(), min.freq = input$userfreq, max.words = input$usermax, scale = c(2.2,1),
              colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)
    
  })
  
  
  output$user_tweet_count <- reactive({
    input$submit_submit
    isolate(prettyNum(user_tweet_count(),big.mark=","))
  })
  
  
  output$keyword <-renderText({
    input$submit
    isolate(l$name)
  })
  
  output$heat_map_post_optimization <- renderPlotly({
    heat_map_post_optimization()
  })
  
  output$username <-renderText({
    input$submit_submit
    isolate(screen_name())
  })
  
  output$average_eng_rate <- renderText({
    input$submit_submit
    isolate(prettyNum(average_eng_rate(),big.mark=","))
  })
  output$num_of_user <- renderText({
    input$submit
    isolate(prettyNum(tweet_user_count(),big.mark=","))
  })
  
  
  output$num_of_followers <- renderText({
    input$submit_submit
    isolate(prettyNum(num_of_followers(),big.mark=","))
  })
  
  output$num_of_retweet <- renderText({
    input$submit
    isolate(prettyNum(tweet_retweet_count(),big.mark=","))
  })
  
  output$num_of_likes <- renderText({
    input$submit
    isolate(prettyNum(tweet_likes_count()+tweet_retweet_count(),big.mark=","))
  })
  
  output$num <- renderText({
    input$submit
    isolate(prettyNum(tweet_count(), big.mark=","))
  })
  
  output$mindate <- renderText({
    input$submit
    isolate(sprintf("From: %s", min_date()))
    #isolate(prettyNum(min_date()))
  })
  
  output$plot_freq_engagement_rate <- renderPlotly({
    plot_freq_engagement_rate()
  })
  
  output$user_senti_plot <- renderPlotly({
    #input$submit_submit
    user_senti_plot()
    #isolate(user_senti_plot())
  })
  
  output$maxdate <- renderText({
    input$submit
    isolate(sprintf("To: %s", max_date()))
    #isolate(prettyNum(max_date()))
  })
  
  output$usermindate <- renderText({
    input$submit_submit
    isolate(sprintf("From: %s",user_min_date()))
  })
  
  
  output$usermaxdate <- renderText({
    input$submit_submit
    isolate(sprintf("To: %s",user_max_date()))
  })
  
  output$tweet_table <- renderReactable({
    input$submit_submit
    isolate(reactable::reactable(user_tweet_table(), 
                                 filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                                 showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,15,20), 
                                 columns = list(
                                   DateTime = colDef(defaultSortOrder = "desc"),
                                   Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                   Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                   RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE))
                                 ))
    )
  })
  
  output$user_positive_table <- renderReactable({
    reactable::reactable(user_positive_table(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,15,20) 
                         #columns = list(
                         #   DateTime = colDef(defaultSortOrder = "desc"),
                         #   Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                         #   Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE))
                         #)
    )
    
  })
  
  output$user_table <- renderReactable({
    reactable::reactable(user_table(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,15,20)
                         # columns = list(
                         #   DateTime = colDef(defaultSortOrder = "desc"),
                         #   Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                         #   Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE))
                         # )
    )
    
  })
  
  
  output$user_negative_table <- renderReactable({
    reactable::reactable(user_negative_table(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10,15,20)
                         # columns = list(
                         #   DateTime = colDef(defaultSortOrder = "desc"),
                         #   Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                         #   Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE))
                         # )
    )
    
  })
  
  output$tweet_fav_positive_table <- renderReactable({
    reactable::reactable(tweet_fav_positive_table(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 10, showPageSizeOptions = TRUE, pageSizeOptions = c(10, 15,20), 
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "desc"),
                           Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE))
                         ))
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

