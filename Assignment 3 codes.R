library(MASS)
library(caTools)
library(neuralnet)
library(ggplot2)

# Import csv file and set seed
graduate <- read.csv("binary.csv")
set.seed(123)

# Split the train / test
split <- sample.split(graduate$admit, SplitRatio = 0.8)
train <- graduate[split, ]
test  <- graduate[!split, ]

# Extract Feature Columns (EXCLUDING target 'admit')
features <- c("gre", "gpa", "rank")

# Scale Continuous Features on TRAIN set
train_scaled_features <- scale(train[, features])
train_center <- attr(train_scaled_features, "scaled:center")
train_scale  <- attr(train_scaled_features, "scaled:scale")

# Convert to data.frame & re-attach unscaled binary 'admit'
train_scaled <- as.data.frame(train_scaled_features)
train_scaled$admit <- train$admit

# Scale TEST set features using TRAIN mean and SD
test_scaled_features <- scale(test[, features], center = train_center, scale = train_scale)
test_scaled <- as.data.frame(test_scaled_features)
test_scaled$admit <- test$admit

# Train Neural Network
set.seed(123)
nn <- neuralnet(
  admit ~ gre + gpa + rank,
  data = train_scaled,
  hidden = 2,
  act.fct = "logistic",
  linear.output = FALSE
)
# Plot the network
plot(nn)

# Generate Predictions on Scaled Test Data
test_preds <- compute(nn, test_scaled[, features])

# Convert Probabilities (net.result) to Binary 0/1 Predictions
pred_class   <- ifelse(test_preds$net.result > 0.5, 1, 0)
actual_class <- test_scaled$admit

# Create Confusion Matrix Table
cm_df <- as.data.frame(table(
  Predicted = factor(pred_class, levels = c(0, 1)),
  Actual    = factor(actual_class, levels = c(0, 1))
))

# Add Quadrant Codes and Combined Label Text for Each Tile
cm_df$quadrant <- c("True Negative", 
                    "False Positive", 
                    "False Negative", 
                    "True Positive")

cm_df$tile_text <- paste0(cm_df$quadrant, "\n\nCount: ", cm_df$Freq)

# Plot Heatmap using ggplot2
ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", lwd = 1.5) +
  geom_text(aes(label = tile_text), color = "white", size = 5.5, fontface = "bold") +
  scale_fill_gradient(low = "#4575b4", high = "#d73027") +
  labs(
    title = "Neural Network Confusion Matrix",
    x = NULL,
    y = NULL,
    fill = "Count"
  ) +
  theme_minimal(base_size = 13)
