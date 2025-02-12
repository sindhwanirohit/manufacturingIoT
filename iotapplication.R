#Application of IoT in Manufacturing
# Install and load necessary libraries
if(!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, caret, randomForest)

# Step 1: Simulate IoT Sensor Data
set.seed(123)
n <- 1000  # Number of observations
sensor_data <- data.frame(
  Time = 1:n,
  Temperature = rnorm(n, mean = 75, sd = 10),
  Vibration = rnorm(n, mean = 0.5, sd = 0.2),
  Power_Consumption = rnorm(n, mean = 150, sd = 20),
  Machine_Failure = sample(c(0, 1), n, replace = TRUE, prob = c(0.95, 0.05))
)

# Step 2: Data Visualization
# Plot Temperature over Time
ggplot(sensor_data, aes(x = Time, y = Temperature)) +
  geom_line(color = "blue") +
  labs(title = "Temperature Sensor Data", x = "Time", y = "Temperature (°F)")

# Plot Vibration over Time
ggplot(sensor_data, aes(x = Time, y = Vibration)) +
  geom_line(color = "red") +
  labs(title = "Vibration Sensor Data", x = "Time", y = "Vibration (g)")

# Plot Power Consumption over Time
ggplot(sensor_data, aes(x = Time, y = Power_Consumption)) +
  geom_line(color = "green") +
  labs(title = "Power Consumption Sensor Data", x = "Time", y = "Power (kW)")

# Step 3: Predictive Maintenance Model
# Split data into training and testing sets
set.seed(456)
trainIndex <- createDataPartition(sensor_data$Machine_Failure, p = 0.7, list = FALSE)
train_data <- sensor_data[trainIndex,]
test_data <- sensor_data[-trainIndex,]

# Train Random Forest Model
model <- randomForest(Machine_Failure ~ Temperature + Vibration + Power_Consumption, 
                      data = train_data, 
                      ntree = 100)

# Model Evaluation
predictions <- predict(model, test_data)
confusionMatrix(as.factor(predictions), as.factor(test_data$Machine_Failure))

# Feature Importance
importance(model)
varImpPlot(model)

# End of Script
