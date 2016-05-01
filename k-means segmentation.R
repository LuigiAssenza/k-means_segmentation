# Libraries needed
library(fpc)

# Upload your data
filePath <- ""

data <- read.csv(filePath, header = TRUE)

# Scale the data. Remove and non-meaningful field (i.e. an ID field)
data <- scale(data)

# Optimanl size of centers that minimizes Within Sum of Squares (WSS)
number_cluster = 5:10 # choose the range

wss <- sapply(number_cluster, function(x) {
  kmeans(data, centers = x)$tot.withinss
})

wss

# Plot the total within-cluster sum of square per every number of cluster in the chosen range 
plot(number_cluster, wss, 
     type="l", 
     xlab = "Number of Clusters", 
     ylab = "Within Group Sum of Squares")

# Highest silhouette level (average silhouette widths)
data_silhouette_width <- sapply(number_cluster, function(x) {
  cluster.stats(dist(data), kmeans(data,x)$cluster)$avg.silwidth
})

# Plot the total mean of the individual silohoutte widths per every number of clusters in the chosen range
plot(number_cluster, data_silhouette_width, 
     type = "l",
     xlab = "Number of Clusters",
     ylab = "Silhoutte")

# What number of clusters the average silhoutte width is maximized
number_centers_opt <- number_cluster[which.max(data_silhouette_width)]

# kmeans optimized
data_kmeans_opt <- kmeans(data, centers = number_centers_opt)

# Custers (segements)
data_kmeans_opt$centers

# Number of elements in each cluster
kmeans(data,data_kmeans_opt$centers)$size