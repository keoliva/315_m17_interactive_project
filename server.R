

########################################################################
#############################   Libraries   ############################
########################################################################

library(dygraphs)
library(ggplot2)
library(shiny)
library(dendextend)
library(reshape2)
library(visNetwork)
library(dplyr)
library(wordcloud)
library(tidytext)
library(tidyr)
library(base)
library(igraph)
library(ggraph)
library(networkD3)
library(geomnet)



theme_315 <- function(angle = 90) { 
  theme_bw() +
  theme(axis.text = element_text(size = 11, color = "dodgerblue4"),
        text = element_text(size = 14, face = "bold", 
                            color = "steelblue4"),
        axis.text.x = element_text(angle = angle, hjust = 1, size = 10)
  )
}


########################################################################
###############################   Data   ###############################
########################################################################

movie <- read.csv("data/movie_metadata.csv", header = T)
movie_cont <- read.csv("data/movie_cont.csv", header = T)
movie_cont <- movie_cont[, -1]

movie_expanded <- read.csv("data/movie_expanded.csv", header = T)

genreChoices <- levels(fct_infreq(movie_expanded$genre)) 


movies <- movie
# transform movie_title column from factor type to character type
movies <-
  mutate(movies, movie_title = sapply(movies$movie_title, as.character))

# transform genres column from factor type to character type
movies <-
  mutate(movies, genres = sapply(movies$genres, as.character))

# transform plot_keywords column from factor type to character type
movies <-
  mutate(movies, plot_keywords = sapply(movies$plot_keywords, as.character))


########################################################################
#########################   Helper Functions   #########################
########################################################################


#------------------------   Word Cloud Helpers  ------------------------#

unnest_plot_keywords <- function() {
  dataset <- movies %>%
    mutate(plot_keywords = strsplit(plot_keywords, '\\|')) %>%
    unnest(plot_keywords)
  return (dataset)
}

movies_plot_keywords <- unnest_plot_keywords()

unnest_genres <- function() {
  dataset <- movies %>%
    mutate(genres = strsplit(genres, '\\|')) %>%
    unnest(genres)
  return (dataset)
}

get_plot_keywords_freq <- function() {
  
  # unnest the plot keywords
  movies_plot_keywords <- unnest_plot_keywords()
  
  # group by plot keywords and count frequency
  plot_keywords_freq <-
    movies_plot_keywords %>%
    dplyr::select(plot_keywords) %>%
    filter(!(is.na(plot_keywords))) %>%
    group_by(plot_keywords) %>%
    summarize(n = n())
  
  return (plot_keywords_freq)
}

plot_keywords_freq <- get_plot_keywords_freq()


get_plot_keywords_avg_gross <- function() {
  
  # unnest the plot keywords
  movies_plot_keywords <- unnest_plot_keywords()
  
  # group by plot keywords and calculate average gross
  plot_keywords_avg_gross <-
    movies_plot_keywords %>%
    dplyr::select(gross, plot_keywords) %>%
    filter(!(is.na(gross))) %>%
    group_by(plot_keywords) %>%
    summarize(avg_gross = mean(gross))
  
  return (plot_keywords_avg_gross)
}

plot_keywords_avg_gross <- get_plot_keywords_avg_gross()


get_plot_keywords_top_freq <- function(dataset, n) {
  new_dataset <-
    dataset %>%
    inner_join(plot_keywords_freq)
  return (new_dataset)
}


plot_keywords_attributes <-
  movies_plot_keywords %>%
  filter(!(is.na(gross) | is.na(budget))) %>%
  filter(!(is.na(num_user_for_reviews) | is.na(movie_facebook_likes))) %>%
  group_by(plot_keywords) %>%
  summarize(avg_gross = mean(gross),
            avg_budget = mean(budget),
            avg_user_reviews = mean(num_user_for_reviews),
            avg_facebook_likes = mean(movie_facebook_likes),
            count = n())

########################################################################
#############################   Server   ###############################
########################################################################


shinyServer(function(input, output) {

##################### helper functions ######################
  
    get_lower_tri <- function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  #----------------------------   Heatmap   ----------------------------#
  
  output$corr_heatmap <- renderPlotly({
    
    cormat <- round(cor(na.omit(movie_cont)), 2)
    cormat <- reorder_cormat(cormat)
    
    cutoff <- as.numeric(input$corr_threshold)
    
    lower_tri_melted <- melt(get_lower_tri(cormat), na.rm = T)
    
    lo <- -1
    hi <- 1
    
    if(cutoff != 0) {
      lo <- min(cormat)
      hi <- max(cormat)
    }
    
    lower_tri_melted <- lower_tri_melted %>%
      mutate(Var1 = fct_recode(Var1, "title year" = "title_year",
                               "facenumber in poster" = "facenumber_in_poster",
                               "actor 1 facebook likes" = "actor_1_facebook_likes",
                               "actor 2 facebook likes" = "actor_2_facebook_likes",
                               "actor 3 facebook likes" = "actor_3_facebook_likes",
                               "director facebook likes" = "director_facebook_likes",
                               "num voted users" = "num_voted_users",
                               "imdb score" = "imdb_score",
                               "num critic for reviews" = "num_critic_for_reviews",
                               "cast total facebook likes" = "cast_total_facebook_likes",
                               "num critic for reviews" = "num_critic_for_reviews",
                               "num user for reviews" = "num_user_for_reviews",
                               "movie facebook likes" = "movie_facebook_likes"
                               ),
             Var2 = fct_recode(Var2, "title year" = "title_year",
                               "facenumber in poster" = "facenumber_in_poster",
                               "actor 1 facebook likes" = "actor_1_facebook_likes",
                               "actor 2 facebook likes" = "actor_2_facebook_likes",
                               "actor 3 facebook likes" = "actor_3_facebook_likes",
                               "director facebook likes" = "director_facebook_likes",
                               "num voted users" = "num_voted_users",
                               "imdb score" = "imdb_score",
                               "num critic for reviews" = "num_critic_for_reviews",
                               "cast total facebook likes" = "cast_total_facebook_likes",
                               "num user for reviews" = "num_user_for_reviews",
                               "movie facebook likes" = "movie_facebook_likes"
             ))
    
    p <- ggplot(data = lower_tri_melted, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white") +
      # geom_tile(aes(color = value < cutoff)) +
      # scale_color_manual(values = c("black", "white"), labels = NULL) +
      geom_text(aes(label = value), color = "black", size = 3) +
      scale_fill_gradient2(low = "darkblue", high = "darkred", mid = "white", 
                           midpoint = cutoff, limit = c(lo, hi), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed() +
      guides(fill = guide_colorbar(barwidth = 1, barheight = 7,
                                   title.position = "bottom")) +
      labs(x = "Variable 1", y = "Variable 2",
           title = "Correlation heap map of continuous variables")
    print(ggplotly(p, height = 1000, width = 1000, 
                   tooltip = c("Var1", "Var2")))
  })

  #----------------------------   Time Series   ----------------------------#
  
  output$time_series_plot <- renderPlot({

    movies_by_year <- reactive({
      filter(movie_expanded, genre %in% input$checkedGenres) %>%
        group_by(title_year, genre) %>% count(title_year) #%>% spread(genre, n)
    })
      
    ggplot(movies_by_year(), aes(x = title_year, y = n)) + geom_line(aes(color = genre)) + 
      labs(title = "Number of Movies Made Throughout the Years")
    
  })
  
  #----------------------------   Dendogram   ----------------------------#
  get_successful_movies <- function(movies) {
    audience_success <- movies[order(-movies$imdb_score, -movies$num_voted_users,
                                     -movies$num_user_for_reviews,
                                     -movies$movie_facebook_likes),]
    
    commercial_success <- movies[order(-movies$gross),]
    
    critic_success <- movies[order(-movies$gross),]
    
    successful_movies <- c(audience_success$movie_title, commercial_success$movie_title)
    
    successful_movies <- c(successful_movies, critic_success$movie_title)
    
    successful_movies <- unique(as.character(successful_movies[duplicated(successful_movies)]))
    
    return(head(successful_movies, n = 30))
  }
  
  base_m <- read_csv("data/base_m.csv")
  
  output$dendrogram <- renderPlot({
    
    movies_00 <- reactive({ filter(base_m, title_year == input$yearSelected) })
    
    successful_movies <- get_successful_movies(movies_00())
    
    successful_movies <- filter(base_m, movie_title %in% successful_movies)
    
    if (input$dendrogramTabs == "audience") {
      
      movies_cont <- dplyr::select(successful_movies, imdb_score, num_voted_users,
                                   num_user_for_reviews, movie_facebook_likes)
      
    } else if (input$dendrogramTabs == "commercial") {
      
      movies_cont <- dplyr::select(successful_movies, gross)
      
    } else {
      
      movies_cont <- dplyr::select(successful_movies, num_critic_for_reviews)
      
    }
    
    dend <- movies_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
    
    colorblind_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    get_colors <- function(x, palette = colorblind_palette) {palette[match(x, unique(x))]}
    
    dend %>% #set("labels_col", get_colors(movies_cont$country)) %>%
      dendextend::set("labels", successful_movies$movie_title, order_value = T) %>%
      ggplot(horiz = T, size = 6) + labs(title = "Clustering Structure of Movies")
  })
  
  #----------------------------   Bar Plot   ----------------------------#
  
  output$bar_plot <- renderPlot({
    
    library(ggplot2)
    library(dplyr)
    library(igraph)
    library(ggraph)
    library(networkD3)
    
    movies <- read_csv("data/imdb.csv")
    
    movies <- movies %>% dplyr::select(content_rating, title_year, gross, budget, imdb_score, duration)
    
    
    
    
    movies <- movies %>% filter(title_year > 1985, content_rating == "PG-13" | content_rating == "G" | content_rating == "R" | content_rating == "PG")
    
    movies <- movies %>% group_by(content_rating) %>% 
      summarise(mean_gross = mean(gross, na.rm = TRUE) ,
                mean_budget = mean(budget, na.rm = TRUE) ,
                mean_score = mean(imdb_score, na.rm = TRUE) ,
                mean_duration = mean(duration, na.rm = TRUE))
    
    if (!input$show_gross) {
      movies <- movies %>% select(-one_of(c("mean_gross")))
    }
    
    if (!input$show_budget) {
      movies <- movies %>% select(-one_of(c("mean_budget")))
    }
    
    if (!input$show_duration) {
      movies <- movies %>% select(-one_of(c("mean_duration")))
    }
    
    if (!input$show_score) {
      movies <- movies %>% select(-one_of(c("mean_score")))
    }
    
    
    
    
    movies <- melt(movies, id.vars = "content_rating")
    
    p <- ggplot(movies, aes(x = content_rating, value)) +  
      geom_bar(aes(fill = variable), position = "dodge", stat="identity")
    
    
    
    return(p)
    
  }, height = 700)
  
  #----------------------------   Word Cloud   ----------------------------#
  
  output$main_plot_wordcloud <- renderPlot ({
    
    # filter the years
    plot_keywords_attributes <-
      movies_plot_keywords %>%
      filter(!(is.na(title_year))) %>%
      filter(!(is.na(gross) | is.na(budget))) %>%
      filter(!(is.na(num_user_for_reviews) | is.na(movie_facebook_likes))) %>%
      filter(title_year >= input$title_year[1] &
               title_year <= input$title_year[2]) %>%
      group_by(plot_keywords) %>%
      summarize(avg_gross = mean(gross),
                avg_budget = mean(budget),
                avg_user_reviews = mean(num_user_for_reviews),
                avg_facebook_likes = mean(movie_facebook_likes),
                count = n())
    
    # filter top n frequent keywords
    if (input$top_n_freq != "All") {
      plot_keywords_attributes <-
        plot_keywords_attributes[order(plot_keywords_attributes$count,
                                       decreasing = TRUE), ] %>%
        head(input$top_n_freq)
    }
    
    plot_keywords_attributes %>%
      with(wordcloud(words = plot_keywords,
                     freq = get(input$variable),
                     scale = c(2, 0.5),
                     rot.per = 0.2,
                     max.words = 60,
                     random.order = FALSE,
                     colors = brewer.pal(6, "RdBu"),
                     vfont = c("sans serif", "bold")))
    title(main = list(paste("Plot Keywords with Most ", input$variable,
                            "from ", input$title_year[1],
                            " to ", input$title_year[2])))
    
    
  })
  
  #------------------------------   Network   -------------------------------#
  
  output$network_plot <- renderVisNetwork({
  
    library(ggplot2)
    library(dplyr)
    library(igraph)
    library(ggraph)
    library(tidyverse)
    library(visNetwork)
    
    movies <- read_csv("data/imdb.csv")
    
    
    grossing <- movies %>% dplyr::select(director_name, gross)
    grossing <- grossing[complete.cases(grossing), ]
    avg_gross <- grossing %>% group_by(director_name) %>%
      dplyr::summarise(mean_gross = mean(gross))
    
    
    gross  <- avg_gross[order(avg_gross$mean_gross,decreasing = TRUE),] 
    
    gross <- head(gross, n = 25)
    
    movies <- movies %>% dplyr::select(director_name, actor_1_name, actor_2_name, actor_3_name)
    movies <-left_join(x = gross, y = movies, by = "director_name")
    
    movies <- movies %>% dplyr::select(director_name, actor_1_name, actor_2_name, actor_3_name) 
    
    movies <- movies[complete.cases(movies), ]
    
    movie_edges <- as.data.frame(matrix(0, ncol = 2, nrow = 0))
    
    # id, label
    movie_vertices <- as.data.frame(matrix(0, ncol = 2, nrow = 0))
    
    movie_edges <- setNames(movie_edges, c("from","to"))
    movie_vertices <- setNames(movie_edges, c("label","id"))
    
    for(i in 1:nrow(movies)) {
      movie <- movies[i,]
      
      director <- movie$director_name
      actor1 <- movie$actor_1_name
      actor2 <- movie$actor_2_name
      actor3 <- movie$actor_3_name
      
      if (!director %in% movie_vertices$label){
        movie_vertices[nrow(movie_vertices) + 1,] = c(director, nrow(movie_vertices) + 1)
      }
      
      if (!actor1 %in% movie_vertices$label){
        movie_vertices[nrow(movie_vertices) + 1,] = c(actor1, nrow(movie_vertices) + 1)
      }
      
      if (!actor2 %in% movie_vertices$label){
        movie_vertices[nrow(movie_vertices) + 1,] = c(actor2, nrow(movie_vertices) + 1)
      }
      
      if (!actor3 %in% movie_vertices$label){
        movie_vertices[nrow(movie_vertices) + 1,] = c(actor3, nrow(movie_vertices) + 1)
      }
      
      
      
      movie_edges[nrow(movie_edges) + 1,] = c(movie_vertices[movie_vertices$label == director, 2],
                                              movie_vertices[movie_vertices$label == actor1, 2])
      
      movie_edges[nrow(movie_edges) + 1,] = c(movie_vertices[movie_vertices$label == director, 2],
                                              movie_vertices[movie_vertices$label == actor2, 2])
      
      movie_edges[nrow(movie_edges) + 1,] = c(movie_vertices[movie_vertices$label == director, 2],
                                              movie_vertices[movie_vertices$label == actor3, 2])
      
      
      
      
    }
  
  visNetwork(movie_vertices, movie_edges)
  })
  
  #------------------------------ Scatter Plot -----------------------------#
  
  output$scatter <- renderPlotly({
    m <- movie_expanded
    m$domestic <- with(m, country == "USA")
    m <- filter(m, 0 < gross & num_voted_users > 0)
    # m <- filter(m, genre %in% input$checkedGenres_scatter) 
    
    # m <- filter(m, content_rating %in% input$checkedRatings & 
    #                title_year == as.numeric(input$year))
    
    m <- filter(m, content_rating %in% input$checkedRatings)
    
    p <- ggplot(m, aes(x = log(num_voted_users), y = log(gross)))
    
    
    if(!input$hide_points){
      p<- p + geom_point(aes(col = content_rating), size = 1, alpha = .6)
    }
    
    if(input$display_overall_spline){
      p <- p + geom_smooth(method = "lm")
    }
    
    if(input$display_genre_spline){
      p <- p + geom_smooth(aes(col = content_rating),
                           method = "lm")
    }
    
    p <- p + theme_bw() +
      scale_color_brewer(type = "qual", palette = "Set2", direction = -1) +
      coord_cartesian() +
      labs(x = "Log transformed number of voted users",
           y = "Log transformed gross",
           title = "Distribution of number of votes versus gross in log scale")
    print(ggplotly(p, height = 600, width = 1000))
  })
  
  #------------------------------ Genres Bar Chart -----------------------------#
  
  output$main_plot_genres_barchart <- renderPlot ({
    
    
    # filter the years
    movies_genres <-
      movies %>%
      filter(!is.na(title_year)) %>%
      filter(input$year_range_genres[1] <= title_year &
               title_year < (input$year_range_genres)[2])
    
    # filter the gross and budget
    movies_genres <-
      movies_genres %>%
      filter((!is.na(gross)) & (!is.na(budget)))
    
    movies_genres <-
      movies_genres %>%
      mutate(genres = strsplit(genres, '\\|')) %>%
      unnest(genres) %>%
      group_by(genres) %>%
      summarise(total_movies = n(),
                total_fb_likes = sum(movie_facebook_likes),
                total_reviews = sum(num_user_for_reviews))
    
    if (input$genres_sorted) {
      movies_genres %>%
        mutate(genres = fct_reorder(genres, get(input$genre_variable))) %>%
        mutate(genres = fct_rev(genres)) %>%
        ggplot(aes(x = genres,
                   y = get(input$genre_variable))) +
        geom_bar(stat = "identity",
                 fill = "brown") +
        labs(x = "Genre",
             y = input$genre_variable,
             title = paste("Distribution of ", input$genre_variable, "of Each Genre",
                           "from ", input$year_range_genres[1],
                           " - ", input$year_range_genres[2])) +
        theme(axis.text.x = element_text(angle = 90))
    } else {
      movies_genres %>%
        ggplot(aes(x = genres,
                   y = get(input$genre_variable))) +
        geom_bar(stat = "identity",
                 fill = "brown") +
        labs(x = "Genre",
             y = input$genre_variable,
             title = paste("Distribution of ", input$genre_variable, "of Each Genre",
                           "from ", input$year_range_genres[1],
                           " - ", input$year_range_genres[2])) +
        theme(axis.text.x = element_text(angle = 90))
    }
    
    
  })

})