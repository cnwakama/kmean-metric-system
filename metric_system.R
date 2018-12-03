library(caret)
library(mlbench)
library(ggplot2)

set.seed(seed)
nstart = 10 # number of random sets to determine a proper center

generator_models <- function(x, k, seed, focusFeature, filterMethod){
  clusters <- list()
  summary(x)
  
  # preprocessing 
  predictor <- preProcess(test, method = c(filterMethod), pcaComp=10, na.remove = TRUE, 
                          k = 5, knnSummary = mean, outcome = NULL, fudge = .2, numUnique = 3)
  View(predictor) # can print(predictory)
  newX <- predict(predictor, x)
  
  # creating multiple cluster
  for (i in length(newX)){
    clusters[i] <- kmeans(x[, c(focusFeature, i)], centers = k, nstart)
  }
  
  ground_cluster <- kmeans(x[, focusFeature], centers = k, nstart)
  print("Analysis")
  # Plot clusters
  for (i in length(clusters)){
    plot(x, col = cluster[1]$cluster)
    table(ground_cluster, clusters[i])
    ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
  }
  
  # Print summary
}

# From the display table and other comparsion information
comparator <- function(listClusters){
  # generate values from cluster
}

# rank features
rank <- function(){
  
}

# creating a score discretely or continuous
metric_system_creator <- function(){
  
} 

# calls the metric weights to make scores on unknown data
scorer <- function(){
  
}