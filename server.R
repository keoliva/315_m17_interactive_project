library(ggplot2)
library(shiny)
library(dendextend)
library(reshape2)
library(visNetwork)


# don't use this for final version
# data_url <- paste("https://raw.githubusercontent.com/sundeepblue/movie_",
#                   "rating_prediction/master/movie_metadata.csv", sep = "")
# movie <- read.csv(data_url, header = T)

movie <- read.csv("data/movie_metadata.csv", header = T)
movie_cont <- read.csv("data/movie_cont.csv", header = T)
movie_cont <- movie_cont[, -1]
theme_315 <- function(angle = 90) { 
  theme_bw() +
  theme(axis.text = element_text(size = 11, color = "dodgerblue4"),
        text = element_text(size = 14, face = "bold", 
                            color = "steelblue4"),
        axis.text.x = element_text(angle = angle, hjust = 1, size = 10)
  )
}

movie_expanded <- read.csv("data/movie_expanded.csv", header = T)


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
  

})