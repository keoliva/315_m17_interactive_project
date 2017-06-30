# File : ui.R
# Project : 36-315 Interactive Project
# Group : Movie Buffs
# Date : June 29, 2017

########################################################################
#############################   Libraries   ############################
########################################################################

library(markdown)
library(plotly)
library(shinydashboard)
library(forcats)

library(tidyverse)

library(ggplot2)
library(shiny)
library(dendextend)
library(reshape2)
library(visNetwork)
library(dplyr)
library(wordcloud)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(base)

library(visNetwork)
library(dygraphs)

########################################################################
#############################   Constants   ############################
########################################################################

movie_expanded <- read.csv("data/movie_expanded.csv", header = T)

genreChoices <- levels(fct_infreq(movie_expanded$genre)) 
ratingChoices <- levels(fct_infreq(movie_expanded$content_rating)) 

# constants

title_year_step = 5
movies <- read.csv("data/movie_metadata.csv", header = T)
min_title_year = min((movies %>% filter(!(is.na(title_year))))$title_year)
max_title_year = max((movies %>% filter(!(is.na(title_year))))$title_year)

title_year_lo = floor(min_title_year / title_year_step) * title_year_step
title_year_hi = ceiling(max_title_year / title_year_step) * title_year_step

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Grouped Bar Graph", icon = icon("th"), tabName = "bar"),
    menuItem("Correlation", icon = icon("th"), tabName = "correlation"),
    menuItem("Time Series", icon = icon("th"), tabName = "time_series"),
    menuItem("Dendrogram", icon = icon("th"), tabName = "dendrogram"),
    menuItem("Network", icon = icon("th"), tabName = "network"),
    menuItem("Wordcloud", icon = icon("th"), tabName = "wordcloud"),
    menuItem("Scatter Plot", icon = icon("th"), tabName = "scatterplot"),
    menuItem("Genres Bar Chart", icon = icon("th"), tabName = "genres_bar")
  )
)

# not using this theme at the moment
css_comps <- tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #f4b943;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #f4b943;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #f4b943;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #f4b943;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ff69b4;
                              }
                              ')
                                  )
                       )


body <- dashboardBody(
  # don't use this theme, default ones are pretty good
  # css_comps,
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(box(includeMarkdown("doc/about.md")),
                     tags$a(img(src="imdb-logo.jpg"), href="https://www.imdb.com"))
    ),
    
    tabItem(tabName = "bar",
            box(title = "Comparison of Movies based on Content Ratings",             
                checkboxInput(inputId = "show_gross", 
                              label = "Show Gross", 
                              value = TRUE),
                
                checkboxInput(inputId = "show_budget", 
                              label = "Show Budget", 
                              value = TRUE),
                
                checkboxInput(inputId = "show_duration", 
                              label = "Show Duration", 
                              value = FALSE),
                
                checkboxInput(inputId = "show_score", 
                              label = "Show IMDB Score", 
                              value = FALSE)), 
            plotOutput(outputId = "bar_plot", height = "600px")
    ),
    
    tabItem(tabName = "correlation",
            fluidRow(             
              box(title = "Correlation Heatmap", status = "primary", 
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE, 
                  plotlyOutput(outputId = "corr_heatmap", width = "auto", height = "auto")
              )
            ),
            fluidRow(box(title = "Input", status = "warning", solidHeader = T,
                         "Default sets correlation 0 as mid-point, which is displayed as white.", br(),
                         "Change the value of mid-point to highlight all grids that have a correlation greater than or equal to this mid-point",
                         sliderInput(
                           label = "Select a mid-point: ",
                           inputId = "corr_threshold",
                           value = 0, min = -1, max = 1, step = 0.2
                         )
                      )
            )
    ),
    tabItem(tabName = "time_series",
            fluidRow(
              selectInput(inputId = "checkedGenres",
                          multiple = T,
                          label = h3("The genres are ordered from most common to rarest."), 
                          choices = genreChoices,
                          selected = list("Drama", "Comedy", "Thriller")
              )
            ),
            fluidRow(
              plotOutput(outputId = "time_series_plot", height = "300px")
            )
    ),
    tabItem(tabName = "dendrogram",
            fluidRow(
              selectInput(inputId = "yearSelected",
                          label = h3("See the movies who achieved about the same 'success'."),
                          choices = c(2000:2016),
                          selected = 2016
              ),
              
              tabsetPanel(id = "dendrogramTabs",
                          tabPanel("Success with the Audience", value = "audience", plotOutput("dendrogram")),
                          tabPanel("Commercial Success", value = "commercial"),
                          tabPanel("Critical Success", value = "critics")
              )
            )
    ),
    #network
    
    tabItem(tabName = "network",
            fluidRow(             
              box(title = "Network", status = "primary", solidHeader = TRUE,
                  width = 12
                  )),
              
           
            fluidPage(
              visNetworkOutput("network_plot", height = "800px"))
            
    ),
            

    # wordcloud
    tabItem(tabName = "wordcloud",
            
            # top row
            fluidRow (
              
              column(width = 5,
                     
                     # select the variable to be visualized
                     selectInput(inputId = "variable",
                                 label = "Variable:",
                                 choices = 
                                   c("Most Frequency" = "count",
                                     "Most Budget" = "avg_budget",
                                     "Most Gross" = "avg_gross",
                                     "Most User Reviews" = "avg_user_reviews",
                                     "Most Movie Facebook Likes" = "avg_facebook_likes"),
                                 selected = "Frequency")
              ),
              
              column(width = 5, offset = 1,
                     
                     # select the top n frequent plot keywords
                     selectInput(inputId = "top_n_freq",
                                 label = "Plot keywords with frequency in top :",
                                 choices = 
                                   c(50, 100, 200, 300, 400, 500, 1000, "All"),
                                 selected = "All")
              )
              
            ),
            
            # main plot
            plotOutput(outputId = "main_plot_wordcloud", height = "400px"),
            
            # bottom row                  
            fluidRow(
              
              column(width = 5,
                     
                     # sliders for year range
                     
                     sliderInput(inputId = "title_year",
                                 label = "Movies produced within year range:",
                                 step = title_year_step,
                                 min = title_year_lo,
                                 max = title_year_hi,
                                 value = c(min, max))
              )
              
            )
    ),
    tabItem(tabName = "scatterplot",
            fluidRow(box(title = "Select Content Rating", status = "warning", solidHeader = T,
                         selectInput(inputId = "checkedRatings",
                                     multiple = T,
                                     label = "Content ratings are ordered from most common to rarest. Select content ratings to dispay:", 
                                     choices = ratingChoices,
                                     selected = list("R", "PG-13", "PG"))),
                     # sliderInput(inputId = "year",
                     #             value = 1993,
                     #             min = 1990, max = 2016, step = 1,
                     #             label = "Choose a year to display")), 
                     box(title = "Control Regression Curve and Points", status = "warning", solidHeader = T,
                         checkboxInput(inputId = "display_genre_spline",
                                       label = "Check to display fitted regression curve for all ratings selected", 
                                       value = F),
                         checkboxInput(inputId = "display_overall_spline",
                                       label = "Check to display fitted regression curve for all movies", 
                                       value = F),
                         checkboxInput(inputId = "hide_points",
                                       label = "Check to hide all points", 
                                       value = F)
                     )),
            fluidRow(box(title = "Scatter Plot", status = "primary", solidHeader = T,
                         width=12,
                         plotlyOutput(outputId = "scatter", height = "auto",
                                      width = "auto")
            ))
    ),
    tabItem(tabName = "genres_bar",
            # top row
            fluidRow (
              
              column(width = 5,
                     
                     # select the variable to be visualized
                     selectInput(inputId = "genre_variable",
                                 label = "Select the feature of genre",
                                 choices = 
                                   c("Facebook Likes" = "total_fb_likes",
                                     "Number of Reviews" = "total_reviews",
                                     "Number of Movies" = "total_movies"),
                                 selected = "total_movies")
              ),
              
              column(width = 5, offset = 1,
                     
                     # select the top n frequent plot keywords
                     checkboxInput(inputId = "genres_sorted",
                                   label = "Sort the genres",
                                   value = FALSE)
              )
              
            ),
            
            # main plot
            plotOutput(outputId = "main_plot_genres_barchart", height = "400px"),
            
            # bottom row                  
            fluidRow(
              
              column(width = 5,
                     
                     # sliders for year range
                     
                     sliderInput(inputId = "year_range_genres",
                                 label = "Year Range",
                                 min = 1990,
                                 max = 2017,
                                 value = c(1990, 2017))
              )
              
            )
    )
  ))

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Movie Buffs"),
  sidebar,
  body
)