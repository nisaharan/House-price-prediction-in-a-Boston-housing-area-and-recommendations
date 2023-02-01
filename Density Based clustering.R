# install the dbscan package
install.packages("dbscan")
# load the dbscan library
library(dbscan)

# exclude the 4th column
data_exclude_4th_column <- Boston_data[,-4]

# Perform density-based clustering using dbscan
dbscan_result <- dbscan(data_exclude_4th_column[,1:ncol(data_exclude_4th_column)-1], eps = 0.5, minPts = 5)
# Assign cluster labels to data
data_exclude_4th_column$cluster <- dbscan_result$cluster


# Visualize the clusters
library(ggplot2)
ggplot(data_exclude_4th_column, aes(x = Boston_data$age, y = Boston_data$medv, color = factor(cluster))) +
  geom_point()
