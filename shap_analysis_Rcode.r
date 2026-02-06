# Load required packages
library(fastshap)
library(tidyverse)
library(shapviz)

# Prepare data
set.seed(123)

# Use complete testing set data
test_sample <- test_set

# Define prediction function (adapted for ranger models)
pred_wrapper <- function(model, newdata) {
  predict(model, data = newdata)$predictions
}
set.seed(123)
# Calculate SHAP values
shap_values <- fastshap::explain(
  final_rf,
  X = test_sample[,-1],  # Remove target variable
  pred_wrapper = pred_wrapper,
  nsim = 500,  # Number of simulations
  adjust = TRUE  # Adjust SHAP values
)

# Create shapviz object
shap_data <- shapviz(shap_values, X = test_sample[,-1])

# Visualization
# 1. Beeswarm plot
# Create beeswarm plot
sv_importance(shap_data, kind = "beeswarm", max_display = 19) +
  # Set global font color to black
  theme(
    text = element_text(color = "black", size = 12, family = "Arial"),            # Global text
    axis.text = element_text(color = "black"),                 # Axis text
    axis.title = element_text(color = "black"),                # Axis titles
    legend.text = element_text(color = "black"),               # Legend text
    # Plot border
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    # Transparent legend background
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent")
  )

# 2. Bar plot
sv_importance(shap_data, kind = "bar", max_display = 19) +
  geom_col(fill = "#94B5D8", show.legend = FALSE) +
  theme(
    # Keep identical font settings to beeswarm plot
    text = element_text(color = "black", size = 12, family = "Arial"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    # Keep identical border settings
    panel.border = element_rect(
      color = "black", 
      fill = NA, 
      linewidth = 1
    )
  )

# Combine the two plots above 
library(patchwork)  # Ensure package is installed

# Create beeswarm plot (plot a)
p_a <- sv_importance(shap_data, kind = "beeswarm", max_display = 19) +
  theme(
    text = element_text(color = "black", size = 14, family = "Arial"),  # Reduce font size
    axis.text = element_text(color = "black", size = 12),  # Reduce axis tick font size
    axis.title = element_text(color = "black", size = 14),  # Reduce axis title font size
    legend.text = element_text(color = "black", size = 12),  # Reduce legend text font size
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    plot.tag = element_text(size = 18, face = "bold"),  # Tag font
    plot.margin = margin(r = 10, t = 10, b = 10)  # Further reduce right and other margins
  ) +
  labs(tag = "a") +  # Add tag "a"
  guides(color = guide_colorbar(
    title = "Feature Value",
    barwidth = 0.8,  # Reduce color bar width
    barheight = 10,  # Reduce color bar height
    title.position = "left",  # Set title position
    title.hjust = 0.5,  # Center title
    title.theme = element_text(size = 15, angle = 90),  # Increase distance between title and color bar
    fill = "transparent"
  )) +
  theme(legend.position = c(0.92, 0.5))  # Set color bar position

# Create bar plot (plot b)
p_b <- sv_importance(shap_data, kind = "bar", max_display = 19) +
  geom_col(fill = "#94B5D8", show.legend = FALSE) +
  theme(
    text = element_text(color = "black", size = 14, family = "Arial"),  # Reduce font size
    axis.text = element_text(color = "black", size = 12),  # Reduce axis tick font size
    axis.title = element_text(color = "black", size = 14),  # Reduce axis title font size
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.tag = element_text(size = 18, face = "bold")  # Tag font
  ) +
  labs(tag = "b")  # Add tag "b"

# Arrange plots side by side, adjust width ratio to make them slightly wider
combined_plot <- p_a + p_b + 
  plot_layout(nrow = 1, widths = c(2, 1.5))  # Make plots wider by increasing width ratio

# Save to desktop, keep overall width at 18cm
ggsave(
  filename = "C:/Users/16421/Desktop/combined_plot3.tif",
  plot = combined_plot,
  device = "tiff",
  dpi = 600,
  width = 30,   # Keep overall width at 18 cm
  height = 30,  # Adjust height to maintain proportions and avoid being too large
  units = "cm"

)
