library(openxlsx)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Read raw data
file_path <- "yourpath/alldata.xlsx" 
pa_data <- read.xlsx(file_path)

# Shuffle data order
set.seed(123)  # Set seed for reproducibility
pa_data <- pa_data[sample(nrow(df)), ]  # Shuffle all rows

# Set column names
colnames(pa_data)[1:4] <- c("lon", "lat", "pa", "ref")  # Ensure first four column names are correct

# Add unique row numbers
pa_data$row_id <- 1:nrow(pa_data)

# Build spatial grid (10° per cell)
pa_data$lon_bin <- floor(pa_data$lon / 10) * 10
pa_data$lat_bin <- floor(pa_data$lat / 10) * 10
pa_data$grid_id <- paste(pa_data$lon_bin, pa_data$lat_bin, sep = "_")

# Extract test set indices: avoid sampling all from sparse grids
set.seed(123)  # Control spatial sampling stability
test_indices <- pa_data %>%
  group_by(grid_id) %>%
  group_modify(~ {
    n <- nrow(.x)
    if (n <= 10) {
      .x %>% slice_sample(n = 1)
    } else {
      k <- ceiling(n * 0.1)
      k <- min(k, ceiling(n * 0.3))
      .x %>% slice_sample(n = k)
    }
  }) %>%
  pull(row_id)

# Construct test and training sets
cols_to_remove <- c("lon_bin", "lat_bin", "grid_id", "row_id")
test_set  <- pa_data[pa_data$row_id %in% test_indices, !(names(pa_data) %in% cols_to_remove)]
train_set <- pa_data[!pa_data$row_id %in% test_indices, !(names(pa_data) %in% cols_to_remove)]

# Plot global spatial distribution of test and training sets
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Construct longitude and latitude ticks
lon_breaks <- seq(-180, 180, by = 30)
lat_breaks <- seq(-90, 90, by = 10)

# Create ggplot layer
p1 <- ggplot(data = world) +
  geom_sf(fill = "gray95", color = "gray70") +
  
  # Training and test set points
  geom_point(data = train_set,
             aes(x = lon, y = lat, color = "Training set"),
             alpha = 0.4, size = 1.5) +
  
  geom_point(data = test_set,
             aes(x = lon, y = lat, color = "Testing set"),
             alpha = 0.8, size = 1.5) +
  
  # Legend color settings
  scale_color_manual(
    values = c("Training set" = "blue", "Testing set" = "red"),
    breaks = c("Training set", "Testing set"),
    labels = c("Training set", "Testing set")
  ) +
  
  # Legend style adjustment
  guides(
    color = guide_legend(override.aes = list(size = 2), title = NULL)
  ) +
  
  # Coordinate range and ticks
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 90),
    expand = FALSE,
    datum = NA
  ) +
  scale_x_continuous(breaks = lon_breaks, limits = c(-180, 180)) +
  scale_y_continuous(breaks = lat_breaks, limits = c(-90, 90)) +
  
  # Theme settings
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Map black border
    panel.background = element_rect(fill = "white", color = NA),  # Main background white
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot area background white
    # Legend settings
    legend.position = "inside",
    legend.position.inside =  c(0.07, 0.5),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.box.background = element_rect(
      color = "black", 
      fill = NA, 
      linewidth = 0.3
    ),
    # Set legend font size (key modification)
    legend.text = element_text(size = 12, color = "black"),  # Adjust legend text size (12 is example value)
    # Eliminate whitespace outside black frame (key modification)
    plot.margin = unit(c(0, 0, 0, 0), "cm")  # Margins for top, right, bottom, left are all 0
  )

# Save image (with cropping settings)
ggsave(
  filename = "yourpath/train_test_map.tif",
  plot = p1,
  dpi = 900,
  width = 16,
  height = 8
)

# Verify data partitioning completeness
stopifnot(nrow(train_set) + nrow(test_set) == nrow(df))

# Visualize PA value distribution histogram
train_set$group <- "Training set"
test_set$group  <- "Testing set"
combined <- rbind(train_set, test_set)
ggplot(combined, aes(x = pa, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  scale_fill_manual(values = c("Training set" = "blue", "Testing set" = "red")) +
  labs(
    x = "Pollen Abundance(%)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    text         = element_text(color = "black"),
    axis.text    = element_text(color = "black", size = 14),
    axis.title   = element_text(color = "black", size = 15),
    axis.ticks   = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(1.5, "mm"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Black frame
    # Set background to white (key modification)
    panel.background = element_rect(fill = "white", color = NA),  # Plot area background
    plot.background = element_rect(fill = "white", color = NA),   # Overall image background
    # Legend settings
    legend.position = c(0.92, 0.5),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    # Set legend font size (key modification)
    legend.text = element_text(size = 11, color = "black"),  # Adjust legend text size
    # Eliminate whitespace outside image (key modification)
    plot.margin = unit(c(0, 0, 0, 0), "mm")  # Set margins to 0 on all sides
  ) +
  guides(
    fill = guide_legend(
      title = NULL,
      reverse = TRUE
    )
  )

# Save image (optimized settings)
ggsave(
  "yourpath/train_test_count.tif",
  plot = last_plot(),
  dpi = 900,
  width = 10,
  height = 6,
  bg = "white" 
)

# Clean up group labels to avoid polluting original objects
train_set$group <- NULL
test_set$group  <- NULL
# Check actual ratios
train_ratio <- nrow(train_set) / (nrow(train_set) + nrow(test_set))
test_ratio  <- nrow(test_set)  / (nrow(train_set) + nrow(test_set))

cat("Train Ratio: ", round(train_ratio, 4), "\n")
cat("Test  Ratio: ", round(test_ratio, 4), "\n")