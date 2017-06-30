

########################################################################
#############################   Libraries   ############################
########################################################################

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
library(networkD3)
library(geomnet)


# don't use this for final version
# data_url <- paste("https://raw.githubusercontent.com/sundeepblue/movie_",
#                   "rating_prediction/master/movie_metadata.csv", sep = "")
# movie <- read.csv(data_url, header = T)


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
  
  output$time_series_plot <- renderPlot({
    
    movies_by_year <- filter(movie_expanded, genre %in% input$checkedGenres) %>% 
      group_by(title_year, genre) %>% count(title_year)
    
    ggplot(movies_by_year, aes(x = title_year, y = n), 
           xlab = "Duration (minutes)",
           main = "Geyser eruption duration") + 
      geom_line(aes(color = genre))
    
  })
  
  
  #----------------------------   Word Cloud   ----------------------------#
  
  output$main_plot_wordcloud <- renderPlot({
    
    wordcloud(words = plot_keywords_freq$plot_keywords,
              freq = plot_keywords_freq$n,
              scale = c(3.5, 0.5),
              rot.per = 0.2,
              random.order = FALSE,
              colors = brewer.pal(8, "Dark2"),
              max.words = 50)
    
  })
  
  #------------------------------   Network   -------------------------------#
  output$network_plot <- renderVisNetwork({
  
    library(ggplot2)
    library(dplyr)
    library(igraph)
    library(ggraph)
    library(tidyverse)
    library(visNetwork)
    
    movies <- read_csv("data/movie_metadata.csv")
    
    
    grossing <- movies %>% dplyr::select(director_name, gross)
    grossing <- grossing[complete.cases(grossing), ]
    avg_gross <- grossing %>% group_by(director_name) %>%
      dplyr::summarise(mean_gross = mean(gross))
    
    
    gross  <- avg_gross[order(avg_gross$mean_gross,decreasing = TRUE),] 
    
    gross <- head(gross, n = 40)
    
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
  
  #------------------------------ Bar Plot -----------------------------#
  output$bar_plot <- renderPlot({
  
    
    movies <- read_csv("data/movie_metadata.csv")
    
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
    
  })
  
  
  
  
  
  
  
  
  
  #------------------------------ Scatter Plot -----------------------------#
  output$scatter <- renderPlotly({
    m <- movie_expanded
    m$domestic <- with(m, country == "USA")
    m <- filter(m, 0 < gross & num_user_for_reviews > 0)
    # m <- filter(m, genre %in% input$checkedGenres_scatter) 
    
    # m <- filter(m, content_rating %in% input$checkedRatings & 
    #                title_year == as.numeric(input$year))
    
    m <- filter(m, content_rating %in% input$checkedRatings)
    
    p <- ggplot(m, aes(x = log(num_user_for_reviews), y = log(gross)))
    
    
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
      coord_cartesian(ylim = c(6, 24)) +
      labs(x = "Log transformed number of reviews",
           y = "Log transformed gross",
           title = "Distribution of number of reviews versus gross")
    print(ggplotly(p, height = 600, width = 1000))
  })

})