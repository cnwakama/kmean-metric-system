library(caret)
library(mlbench)
library(ggplot2)

# set.seed(seed)
nstart = 10 # number of random sets to determine a proper center

prepareData <- function(x, filter, method) {
  df = x[,!(names(x) %in% filter)]
  
  print('Statistics of Previous dataset')
  print(summary(df))
  
  # preprocessing 
  print('Preprocessing...')
  predictor <- preProcess(df, method = method, pcaComp=10, na.remove = TRUE, 
                          k = 5, knnSummary = mean, outcome = NULL, fudge = .2, numUnique = 3)
  print(predictor) # can print(predictory)
  df <- predict(predictor, df)
  
  print(c('New Statistics with', method))
  print(summary(df))
  
  return(df)
}

generator_models <- function(x, k, focusFeature){
  clusters <- vector('list')
  
  # creating multiple cluster
  for (i in length(x)){
    # print('it works');
    clusters[i] <- kmeans(x[,(names(x) %in% c(focusFeature))], centers = k, 20)
  }
  
  ground_cluster <- kmeans(x[,(names(x) %in% focusFeature)], centers = k, 20)
  
  #plot(x, col = ground_cluster$cluster)
  # print("Analysis")
  # Plot clusters
  # for (i in length(clusters)){
  #   plot(x, col = cluster[i]$cluster)
  #   # table(ground_cluster, clusters[i])
  #   # ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
  # }
  
  return(clusters)
}

# From the display table and other comparsion information
comparator <- function(listClusters, ground, threshold){
  # generate values from cluster
  # variance and distance (2 verisons)
  
  # list <- listClusters[i]$centers
  importantClusters <- vector('list')
  weakClusters <- vector('list')
  groundClusters <- vector('list')
  
  g <- ground$betweenss / ground$tot.withinss
  
  for (i in length(listClusters)){
    # compare variance to see if what features inprove variance from ground cluster
    k <- listClusters[i]$betweenss / listClusters[i]$tot.withinss
    
    if (k > g){
      importantClusters <- append(importantClusters, listClusters[i])
    }
    else if (theshold > abs(k - g)) {
      groundClusters <- append(groundClusters, listClusters[i])
    }
    else {
      weakClusters <- append(weakClusters, listClusters[i])
    }
  }
}

# rank features
rank <- function(feature1, feature2, feature3) {
  rank <- vector('list')
  
  f1 <- feature1[order(feature1[1]), ]
  f2 <- feature2[order(feature2[1]), ]
  f3 <- feature3[order(feature3[1]), ]
  
  f <- list(c(f1, f2, f3))
  
}

# creating a score discretely or continuous
metric_system_creator <- function(weights, features) {
  metric_score <- data.frame() # vector('list')
  for (i in length(weights)) {
    metric_score[[features[i]]] <- weights[i]
  }
} 

# calls the metric weights to make scores on unknown data
scorer <- function(metric_score, input) {
  score <- 0
  
  for (i in length(input)){
    score <- score + metric_score[[ input[[i]][1] ]] * input[[i]][2]
  }
}