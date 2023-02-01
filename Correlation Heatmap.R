#Correlation HeatMap1
library(corrplot)

# Replace "data" with the name of your dataframe
corr_matrix <- cor(Boston_data)

# Plot the correlation map
corrplot(corr_matrix, method = "square")
