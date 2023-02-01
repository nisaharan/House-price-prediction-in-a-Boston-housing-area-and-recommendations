# Calculate the first and third quartiles
Q1 <- quantile(Boston_data$medv, 0.25)
Q3 <- quantile(Boston_data$medv, 0.75)

# Calculate the interquartile range
IQR <- Q3 - Q1

# Identify outliers
outliers <- Boston_data[Boston_data$medv < (Q1 - 1.5 * IQR) | Boston_data$medv > (Q3 + 1.5 * IQR), ]

# Print the outliers
print(outliers)
