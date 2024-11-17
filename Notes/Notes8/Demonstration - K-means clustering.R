setwd("E:\\TEACHING\\714\\")
df <- read.csv("weather_data.csv")

#Question: how do I drop the 'X' column?

head(df)
df <- df[,-1]

#we have NA values in the data, which messes things up
#Let's infill NA values with previous valid value

infill_na_with_last_valid <- function(data, column_name) {
  last_valid_value <- NA
  for(i in 1:nrow(data)) {
    if(is.na(data[i,column_name])) {
      if (!is.na(last_valid_value)) {
        data[i,column_name] <- last_valid_value
      }
    } else {
      last_valid_value <- data[i,column_name]
    }
  }
  return(data[,column_name])
}

df$max_temp <- infill_na_with_last_valid(df,"max_temp")
df$mean_temp <- infill_na_with_last_valid(df,"mean_temp")
df$min_temp <- infill_na_with_last_valid(df,"min_temp")
df$total_precip <- infill_na_with_last_valid(df,"total_precip")

#scaled and re-structured
cluster_data <- scale(df[, c('max_temp', 'mean_temp', 'min_temp', 'total_precip')])

#The WSS calculates the sum of the squared distances between each data
#point within a cluster and the centroid of that cluster. As the number
#of clusters increases, the WSS typically decreases because the data
#points are closer to the centroids of their respective clusters. However,
#after a certain point, the reduction in WSS becomes marginal, creating an "elbow"
#in the plot of WSS versus the number of clusters. Identifying the location of this
#elbow helps to select an appropriate number of clusters where the reduction in WSS
#starts to diminish, ensuring that the clustering is neither too complex (with too
#many clusters) nor too simplistic (with too few). This balance makes the WSS
#method a powerful heuristic for defining the underlying structure in a dataset without
#overfitting or underfitting.

wss <- (nrow(cluster_data)-1)*sum(apply(cluster_data,2,var))

for (i in 2:15) {
  kmeans_model <- kmeans(cluster_data, centers=i)
  wss[i] <- kmeans_model$tot.withinss
}

#it would be nice if there was an obvious 'kink'...
plot(1:15, wss, type='b', xlab="Number of Clusters", ylab="WSS")

#K-means
kmeans_model <- kmeans(cluster_data, centers=2)

time <- seq(1:nrow(cluster_data))
cluster_data <- cbind(time,cluster_data)

#clusters around seasons--rainiy and dry?
plot(cluster_data[, c('time', 'total_precip')],
     col=kmeans_model$cluster,
     pch=20, main="K-means Clustering",
     xlab="Time",
     ylab="Mean temperature",
     cex=0.1)
