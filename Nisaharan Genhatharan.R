#installing Packages
install.packages("caret") 
library(caret)
library(tidyverse)
library(car) #for VIF

Boston_data <- read.csv(file = "housing.csv", header=T)
view(Boston_data)

install.packages("skimr")
library(skimr)
summaryStats <- skim(Boston_data)
summaryStats

boxplot(Boston_data$medv~Boston_data$chas, ylab="Median house price")

# Install and load reshape2 package
install.packages("reshape2")
library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(Boston_data),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

# Load the required library
library(ggplot2)
library(ggthemes)

# Iterate through each predictor variable
for(i in 1:(ncol(Boston_data)-1)) {
  # Skip the 4th column
  if(i == 4) next
  
  # Create a scatter plot for the current predictor variable against the response variable
  p <- ggplot(Boston_data, aes(x=!!as.name(colnames(Boston_data)[i]), y=!!as.name(colnames(Boston_data)[13]))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_tufte()+
    ggtitle(paste0("Scatter plot of ", colnames(Boston_data)[i], " vs ", colnames(Boston_data)[13]))
  
  # Save the current plot as an image file
  ggsave(filename = paste0(colnames(Boston_data)[i], ".jpeg"), plot = p)
}


# Histogram
for(i in 1:ncol(Boston_data)) {
  #create a histogram for current variable
  p <- ggplot(Boston_data, aes(!!as.name(colnames(Boston_data)[i]))) +
    geom_histogram(binwidth = 1) +
    ggtitle(paste0("Histogram of ", colnames(Boston_data)[i]))
  print(p)
}

# Density plot
for(i in 1:(ncol(Boston_data)-1)) {
  # Skip the 4th column
  if(i == 4) next
  
  # Create a density plot for the current predictor variable
  p <- ggplot(Boston_data, aes(x=!!as.name(colnames(Boston_data)[i]))) +
    geom_density() +
    theme_tufte()+
    ggtitle(paste0("Density plot of ", colnames(Boston_data)[i]))
  
  # Save the current plot as an image file
  ggsave(filename = paste0(colnames(Boston_data)[i], ".jpeg"), plot = p)
}


# Calculate the first and third quartiles
Q1 <- quantile(Boston_data$medv, 0.25)
Q3 <- quantile(Boston_data$medv, 0.75)

# Calculate the interquartile range
IQR <- Q3 - Q1

# Identify and remove outliers
Boston_data_without_outliers <- Boston_data[!(Boston_data$medv < (Q1 - 1.5 * IQR) | Boston_data$medv > (Q3 + 1.5 * IQR)), ]



#Step 1: Partition our data
set.seed(99) #set random seed
index <- createDataPartition(Boston_data_without_outliers$medv, p = .8,list = FALSE)
Boston_train <-Boston_data_without_outliers[index,]
Boston_test <- Boston_data_without_outliers[-index,]

#Step 2: Train or fit the model
housing_model <- train(medv ~ .,
                       data = Boston_train,
                       method = "lm",
                       trControl =trainControl(method = "none"))
#Get results                             
summary(housing_model)

#Step 3: Predictions using testing data
medv_pred<-predict(housing_model, Boston_test)
MSE <- mean((medv_pred-Boston_test$medv)^2)
MSE

# Create a box plot
boxplot(Boston_data$crim, main="Boxplot of crim", xlab="crim")

# Identify and mark any outliers
outliers <- boxplot.stats(Boston_data$crim)$out
points(outliers,pch=22,bg="red")

library(knitr)
knit("Full code.R", output = "Code.pdf")

