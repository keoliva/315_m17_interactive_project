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

movie_expanded <- read.csv("data/movie_expanded.csv", header = T)

genreChoices <- levels(fct_infreq(movie_expanded$genre)) 
ratingChoices <- levels(fct_infreq(movie_expanded$content_rating)) 

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Item 3", icon = icon("th"), tabName = "correlation"),
    menuItem("Item 4", icon = icon("th"), tabName = "time_series"),
    menuItem("Item 5", icon = icon("th"), tabName = "network"),
    menuItem("Item 6", icon = icon("th"), tabName = "wordcloud"),
    menuItem("Item 7", icon = icon("th"), tabName = "scatterplot")
  )
)

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
    
    #network
    
    tabItem(tabName = "network",
            fluidRow(             
              box(title = "Network", status = "primary", solidHeader = TRUE,
                  width = 12,
                  selectInput(inputId = "v_method",
                                        label = "Visualization Method:",
                                        choices = c("Fruchterman–Reingold" = "fr", 
                                                    "Kamada-Kawai" = "kk"),
                                        selected = "kk"))),
              
           
              plotOutput(outputId = "network_plot", height = "300px")
            
    ),
            

    # wordcloud
    tabItem(tabName = "wordcloud",
            
            # top row
            fluidRow (
              
              # checkboxes for contour map, heat map, and scatter plot
              column(width = 5, offset = 1)
              
            ),
            
            # main plot
            plotOutput(outputId = "main_plot_wordcloud", height = "300px"),
            
            # bottom row                  
            fluidRow(
              
              # sliders for contour map and heat map
              column(width = 5, offset = 1)
              
            )
    ),
    tabItem(tabName = "scatterplot",
            fluidRow(box(title = "Select Content Rating and year", status = "warning", solidHeader = T,
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
    )
  ))
  


# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Movie Buffs"),
  sidebar,
  body
)