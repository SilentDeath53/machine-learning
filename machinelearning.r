library(caret)     
library(ggplot2)   

data <- read.csv("dataset.csv")

data[, c("numeric_var1", "numeric_var2")] <- scale(data[, c("numeric_var1", "numeric_var2")])

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$target_variable, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

model <- train(target_variable ~ ., data = trainData, method = "rf")

# Make predictions on the test set
predictions <- predict(model, newdata = testData)

confusionMatrix(predictions, testData$target_variable)

ggplot(data = testData, aes(x = predicted_values, y = actual_values)) +
  geom_point() +
  labs(title = "Predicted vs. Actual Values")

ggsave("scatter_plot.png", plot = last_plot(), width = 6, height = 4)
