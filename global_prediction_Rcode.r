# Load required packages
library(terra)
library(ranger)
library(fs)  # For file system operations

# Log function - for recording key steps and errors
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] [", level, "] ", msg, "\n"))
}

# Get all subfolder paths
get_subfolders <- function(parent_dir) {
  if (!dir.exists(parent_dir)) {
    stop(paste("Error: Parent directory does not exist -", parent_dir))
  }
  
  subfolders <- dir_ls(parent_dir, type = "directory")
  if (length(subfolders) == 0) {
    stop(paste("Error: No subfolders found in directory -", parent_dir))
  }
  
  return(subfolders)
}

# Prepare raster data for a single subfolder
prepare_raster_data <- function(raster_dir, train_data) {
  log_message(paste("Starting processing of subfolder:", raster_dir))
  
  # Read all raster files
  log_message("Reading raster files...")
  climate_rasters <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(climate_rasters) == 0) {
    log_message(paste("Warning: No TIFF raster files found in subfolder -", raster_dir), "WARNING")
    return(NULL)
  }
  
  # Stack raster layers
  stacked_rasters <- tryCatch({
    rast(climate_rasters)
  }, error = function(e) {
    log_message(paste("Raster stacking failed:", e$message), "ERROR")
    return(NULL)
  })
  
  log_message(paste("Successfully stacked", nlyr(stacked_rasters), "raster layers"))
  
  # Extract filenames as layer names
  layer_names <- tools::file_path_sans_ext(basename(climate_rasters))
  names(stacked_rasters) <- layer_names
  
  # Print updated layer names
  log_message("Updated raster layer names (based on filenames):")
  print(names(stacked_rasters))
  
  # Get model predictors (exclude response variable "pa")
  predictors <- names(train_data)[!names(train_data) %in% "pa"]
  log_message("Variable order in model:")
  print(predictors)
  
  # Check if all predictors exist in raster layers
  missing_vars <- setdiff(predictors, names(stacked_rasters))
  if (length(missing_vars) > 0) {
    log_message(paste("Error: Raster data missing the following model variables:", paste(missing_vars, collapse = ", ")), "ERROR")
    return(NULL)
  }
  
  # Reorder raster layers to match model variable order
  tryCatch({
    stacked_rasters <- stacked_rasters[[predictors]]
    log_message("Final ordered raster layer names:")
    print(names(stacked_rasters))
    
    # Verify ordering
    if (!all(names(stacked_rasters) == predictors)) {
      log_message("Warning: Raster layer order does not perfectly match model variable order", "WARNING")
    }
    
    return(stacked_rasters)
  }, error = function(e) {
    log_message(paste("Raster layer reordering failed:", e$message), "ERROR")
    return(NULL)
  })
}

# Perform ten model trainings and predictions, then calculate averages
run_ensemble_prediction <- function(raster_stack, train_data, scenario_dir) {
  log_message(paste("Starting ensemble prediction process:", basename(scenario_dir)))
  
  # Create output directory in subfolder
  ensemble_dir <- file.path(scenario_dir, "ensemble_predictions")
  dir.create(ensemble_dir, showWarnings = FALSE)
  
  # Initialize vector to save paths of each model prediction
  ensemble_paths <- c()
  
  # Perform ten trainings and predictions
  for (i in 1:10) {
    log_message(paste("Starting", i, "th prediction..."))
    
    # Set random seed for reproducibility
    set.seed(1000 + i)
    
    # Train random forest model
    log_message("Training random forest model...")
    tryCatch({
      model_i <- ranger(
        pa ~ .,
        data = train_data,
        mtry = 3,
        min.node.size = 10,
        sample.fraction = 0.776,
        num.trees = 1000,
        importance = "permutation",
        verbose = FALSE
      )
      
      # Set prediction output path
      pred_path_i <- file.path(ensemble_dir, paste0("prediction_", i, ".tif"))
      ensemble_paths[i] <- pred_path_i
      
      # Execute prediction
      log_message(paste("Performing prediction (", i, "/10)...", sep = ""))
      suppressWarnings({
        prediction <- terra::predict(
          object = raster_stack,
          model = model_i,
          filename = pred_path_i,
          type = "response",
          na.rm = TRUE,
          overwrite = TRUE,
          progress = "text"
        )
      })
      
      log_message(paste(i, "th prediction completed and saved to:", pred_path_i))
      
    }, error = function(e) {
      log_message(paste(i, "th prediction failed:", e$message), "ERROR")
    })
  }
  
  # Check for successful prediction results
  if (length(ensemble_paths) > 0 && all(file.exists(ensemble_paths))) {
    # Read all predictions and calculate mean
    log_message("Reading all prediction results and calculating average...")
    pred_rasters <- rast(ensemble_paths)
    mean_prediction <- mean(pred_rasters, na.rm = TRUE)
    
    # Save mean prediction result
    mean_output_path <- file.path(ensemble_dir, "mean_prediction.tif")
    writeRaster(
      mean_prediction,
      filename = mean_output_path,
      overwrite = TRUE
    )
    
    log_message(paste("Ensemble prediction average saved to:", mean_output_path))
    return(mean_output_path)
  } else {
    log_message("Warning: No successful prediction results, cannot calculate average", "WARNING")
    return(NULL)
  }
}

# Main function - batch process all subfolders
batch_process_future_climate <- function(parent_dir, train_data) {
  log_message("Starting batch processing of future climate scenario predictions...")
  
  # Get all subfolders
  subfolders <- get_subfolders(parent_dir)
  log_message(paste("Found", length(subfolders), "subfolders to process"))
  
  # Initialize results path list
  results <- list()
  
  # Process each subfolder
  for (folder in subfolders) {
    # Prepare raster data
    raster_stack <- prepare_raster_data(folder, train_data)
    
    if (!is.null(raster_stack)) {
      # Execute ensemble prediction (results saved in subfolder)
      result_path <- run_ensemble_prediction(raster_stack, train_data, folder)
      results[[basename(folder)]] <- result_path
    }
  }
  
  log_message("Batch processing complete!")
  return(results)
}

# Execute batch processing
# Assuming train_set is already defined in the environment
future_climate_dir <- "/data/h01016/2.5m_future_climate/"

# Check if train_set exists
if (!exists("train_set")) {
  stop("Error: Training dataset 'train_set' not found, please ensure this dataset is defined")
}

# Start batch processing
results <- batch_process_future_climate(future_climate_dir, train_set)

# Print results summary
log_message("Prediction results summary:")
print(results)