library(markdown)
library(plotly)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Item 1", icon = icon("th"), tabName = "boxplot",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Item 2", icon = icon("th"), tabName = "dendrogram",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Item 3", icon = icon("th"), tabName = "correlation",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Item 4", icon = icon("th"), tabName = "time_series",
             badgeLabel = "new", badgeColor = "green")
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
    
    tabItem(tabName = "boxplot",
            fluidRow(             
              box(
                title = "Boxplot", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 12, 
                plotlyOutput("boxplot", width = "auto", height = "auto")
              )
            ),
            fluidRow(
              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                "By default, boxplot for each country are sorted by median", 
                br(), "More sorting options are supported",
                selectInput(
                  label = "Select Sorting Criteria:",
                  inputId = "boxplot_sort",
                  selected = "by median",
                  choices = c("by median", "by count", "by max", "by min"))
              )
            )
    ),
    tabItem(tabName = "dendrogram",
            fluidRow(             
              box(title = "Dendrogram", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 12,
                  plotOutput("dendrogram")
              )
            )
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
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Movie Buffs"),
  sidebar,
  body
)