

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
  
  #################### plots #####################
  output$boxplot <- renderPlotly({
    if(input$boxplot_sort == "by median") {
      f <- median
    }
    if(input$boxplot_sort == "by min"){
      f <- min
    }
    if(input$boxplot_sort == "by max") {
      f <- max
    }
    if(input$boxplot_sort == "by count"){
      f <- length
    }    
    
    p <- ggplot(movie, 
                aes(x = reorder(country, imdb_score, FUN=f), 
                    y = imdb_score)) + 
      geom_boxplot() + theme_315(90) +
      labs(x = "Countries", y = "IMDB rating", 
           title = "Distribution of IMDB rating across countries")
    print(ggplotly(p, height = 500, width = 1000))
  })
  
  output$dendrogram <- renderPlot({
    dend <- t(movie_cont) %>% scale %>% dist %>% hclust %>% as.dendrogram
    p <- ggplot(dend, horiz = T)
    return(p)
  })
  
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
  

})