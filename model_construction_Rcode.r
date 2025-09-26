# Building the Final Model
set.seed(123)  # Set random seed for reproducibility
library(ranger)  # Load the ranger package
final_rf <- ranger(
  pa ~.,  
  data = train_set,  
  mtry = 3,  
  min.node.size = 10,  
  sample.fraction = 0.776, 
  num.trees = 1000,  
  importance = "permutation"  
)

# Test Set Validation 

# Code for 10-iteration Average of MAE, RMSE, R² 
# Load required packages
library(ranger)
library(Metrics)

# Initialize results storage
results <- data.frame(
  iteration = integer(10),
  train_r2 = numeric(10),
  test_r2 = numeric(10),
  train_rmse = numeric(10),
  test_rmse = numeric(10),
  train_mae = numeric(10),
  test_mae = numeric(10)
)

# Perform 10 repeated trainings
for (i in 1:10) {
  set.seed(123 + i)  # Set different seed for each iteration
  
  # Train the model
  final_rf <- ranger(
    formula = pa ~ .,
    data = train_set,
    mtry = 3,
    min.node.size = 10,
    sample.fraction = 0.776,
    num.trees = 1000,
    importance = "permutation"
  )
  
  # Make predictions on training and test sets
  pred_train <- predict(final_rf, data = train_set)$predictions
  pred_test <- predict(final_rf, data = test_set)$predictions
  
  # Get true values
  true_train <- train_set$pa
  true_test <- test_set$pa
  
  # Calculate metrics
  results[i, "iteration"] <- i
  results[i, "train_r2"] <- 1 - (sum((true_train - pred_train)^2) / sum((true_train - mean(true_train))^2))
  results[i, "test_r2"] <- 1 - (sum((true_test - pred_test)^2) / sum((true_test - mean(true_test))^2))
  
  results[i, "train_rmse"] <- rmse(true_train, pred_train)
  results[i, "test_rmse"] <- rmse(true_test, pred_test)
  
  results[i, "train_mae"] <- mae(true_train, pred_train)
  results[i, "test_mae"] <- mae(true_test, pred_test)
}

# Calculate average results
avg_results <- colMeans(results[, -1])  # Exclude the iteration column

# Output average results
print("Average evaluation results (mean of 10 trainings)")
avg_results

# Plotting Scatter Plots 
library(ranger)
library(caret)
library(ggplot2)
library(patchwork)
library(extrafont)

# Initialize prediction storage
train_preds <- matrix(NA, nrow = nrow(train_set), ncol = 10)
test_preds <- matrix(NA, nrow = nrow(test_set), ncol = 10)

# Perform 10 trainings and store predictions each time
for (i in 1:10) {
  set.seed(123 + i)
  
  # Train the model
  final_rf <- ranger(
    formula = pa ~ .,
    data = train_set,
    mtry = 3,
    min.node.size = 10,
    sample.fraction = 0.776,
    num.trees = 1000,
    importance = "permutation",
    seed = 123 + i
  )
  
  # Store predictions from each iteration
  train_preds[, i] <- predict(final_rf, data = train_set)$predictions
  test_preds[, i] <- predict(final_rf, data = test_set)$predictions
  
  # Calculate average predictions for training and test sets
  avg_train_pred <- rowMeans(train_preds)
  avg_test_pred <- rowMeans(test_preds)
  
  # Get true values
  true_train <- train_set$pa
  true_test <- test_set$pa
  
  # Calculate R², MAE, and RMSE for training set
  train_r2 <- cor(avg_train_pred, true_train)^2
  train_mae <- mean(abs(avg_train_pred - true_train))
  train_rmse <- sqrt(mean((avg_train_pred - true_train)^2))
  
  # Calculate R², MAE, and RMSE for test set
  test_r2 <- cor(avg_test_pred, true_test)^2
  test_mae <- mean(abs(avg_test_pred - true_test))
  test_rmse <- sqrt(mean((avg_test_pred - true_test)^2))
  
  # Output current evaluation metrics
  cat("Training iteration", i, ":\n")
}

# Set position parameters for y=x reference line label
abline_label_pos <- "topright"  # Optional values: "topleft", "topright", "bottomleft", "bottomright"

# Fine-tuning parameters: positive values move right/up, negative values move left/down
x_offset <- -0.15  # Horizontal offset (proportion of x range)
y_offset <- 0.06   # Vertical offset (proportion of y range)

# Create training set scatter plot with labels (a)
avg_train_plot_data <- data.frame(
  true_values = true_train,
  avg_predictions = avg_train_pred
)

# Determine position for y=x label based on parameters
get_abline_label_position <- function(pos, true_data, pred_data, x_offset = 0, y_offset = 0) {
  x_min <- min(true_data)
  x_max <- max(true_data)
  y_min <- min(pred_data)
  y_max <- max(pred_data)
  
  x_range <- x_max - x_min
  y_range <- y_max - y_min
  
  base_pos <- switch(pos,
                     "topleft" = list(x = x_min + x_range * 0.1, y = y_min + y_range * 0.9),
                     "topright" = list(x = x_min + x_range * 0.9, y = y_min + y_range * 0.9),
                     "bottomleft" = list(x = x_min + x_range * 0.1, y = y_min + y_range * 0.1),
                     "bottomright" = list(x = x_min + x_range * 0.9, y = y_min + y_range * 0.1),
                     stop("Invalid position specified")
  )
  
  # Apply offsets
  base_pos$x <- base_pos$x + x_range * x_offset
  base_pos$y <- base_pos$y + y_range * y_offset
  
  return(base_pos)
}

train_label_pos <- get_abline_label_position(abline_label_pos, true_train, avg_train_pred, x_offset, y_offset)
test_label_pos <- get_abline_label_position(abline_label_pos, true_test, avg_test_pred, x_offset, y_offset)

# Remaining code remains unchanged...
train_plot <- ggplot(avg_train_plot_data, aes(x = true_values, y = avg_predictions)) +
  geom_point(color = "blue", size = 2) +  
  labs(x = "True Values", y = "Average Predicted Values") +
  geom_abline(slope = 1, intercept = 0, color = "skyblue", linewidth = 1) + 
  # Add y=x label
  annotate("text", x = train_label_pos$x, y = train_label_pos$y, 
           label = "y = x", color = "skyblue", size = 6, fontface = "bold") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    text = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial", color = "black", size = 15),
    axis.text = element_text(family = "Arial", color = "black", size = 14),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(0.15, "cm"),
    plot.tag = element_text(face = "bold", size = 25)  # Set label style
  ) + 
  labs(tag = "(a)") +  # Add label
  # Add metrics text in top-left corner
  annotate("text", x = min(true_train), y = max(avg_train_pred), 
           label = paste(" R² = ", round(train_r2, 2), "\n",
                         "MAE = ", round(train_mae, 2), "\n",
                         "RMSE = ", round(train_rmse, 2)), 
           hjust = 0, vjust = 1, size = 5, color = "black")

# Create test set scatter plot with labels (b)
avg_test_plot_data <- data.frame(
  true_values = true_test,
  avg_predictions = avg_test_pred
)

test_plot <- ggplot(avg_test_plot_data, aes(x = true_values, y = avg_predictions)) +
  geom_point(color = "red", size = 2) +  
  labs(x = "True Values", y = "Average Predicted Values") +
  geom_abline(slope = 1, intercept = 0, color = "skyblue", linewidth = 1) + 
  # Add y=x label
  annotate("text", x = test_label_pos$x, y = test_label_pos$y, 
           label = "y = x", color = "skyblue", size = 6, fontface = "bold") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    text = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial", color = "black", size = 15),
    axis.text = element_text(family = "Arial", color = "black", size = 14),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(0.15, "cm"),
    plot.tag = element_text(face = "bold", size = 25)  # Set label style
  ) + 
  labs(tag = "(b)") +  # Add label
  # Add metrics text in top-left corner
  annotate("text", x = min(true_test), y = max(avg_test_pred), 
           label = paste(" R² = ", round(test_r2, 2), "\n",
                         "MAE = ", round(test_mae, 2), "\n",
                         "RMSE = ", round(test_rmse, 2)), 
           hjust = 0, vjust = 1, size = 5, color = "black")

# Combine plots using patchwork
combined_plot <- train_plot + test_plot + 
  plot_annotation(tag_levels = 'a') 

# Save path
output_path <- "yourpath/combined_plot.tif"

# Save the plot
ggsave(output_path, combined_plot, width = 16, height = 8, dpi = 900)

# Variable Importance (Random Forest Built-in) 
library(extrafont)

# Import system fonts (first run may take several minutes)
font_import()  # To import only Arial: font_import(pattern = "Arial")

# Load font database
loadfonts()

# View available fonts
fonts()

# Extract variable importance
var_importance <- final_rf$variable.importance

# Convert to dataframe and sort
var_importance_df <- data.frame(
  variable = names(var_importance),
  importance = as.numeric(var_importance)
) %>%
  arrange(desc(importance))

# Load required packages
library(ggplot2)
library(dplyr)

# Plot variable importance
importance_plot <- ggplot(var_importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Horizontal bar chart
  labs(
    x = "Features in Model",
    y = "Importance (Permutation)"
  ) +
  theme_minimal(base_family = "Arial") +  # Set global font to Arial
  theme(
    # Axis titles: black, size 15
    axis.title.x = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 15, color = "black"),
    
    # Axis tick labels: black
    axis.text.x = element_text(size = 14, color = "black"),  # X-axis text
    axis.text.y = element_text(size = 14, color = "black"),  # Y-axis text
    
    # Border settings
    panel.border = element_rect(color = "black", size = 1, fill = NA),
    
    # Set background to white (critical modification)
    panel.background = element_rect(fill = "white", color = NA),  # Plot area background
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot background
    
    # Remove external margins (critical modification)
    plot.margin = unit(c(0, 0, 0, 0), "cm")  # Top, right, bottom, left margins set to 0
  )

# Display the plot
print(importance_plot)

# Save as 900 dpi TIF file
ggsave(
  filename = "yourpath/variable_importance.tif",
  plot = importance_plot,
  dpi = 900,
  width = 7,   # Adjust width as needed
  height = 8,  # Adjust height as needed
  bg = "white"  # Ensure white background when saving
)