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


# loop through all variables in the data frame
for(i in 1:ncol(Boston_data)) {
  #create a histogram for current variable
  p <- ggplot(Boston_data, aes(!!as.name(colnames(Boston_data)[i]))) +
    geom_histogram(binwidth = 1) +
    ggtitle(paste0("Histogram of ", colnames(Boston_data)[i]))
  print(p)
}