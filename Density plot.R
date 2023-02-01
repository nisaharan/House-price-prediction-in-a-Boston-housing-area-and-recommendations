# Iterate through each predictor variable
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
