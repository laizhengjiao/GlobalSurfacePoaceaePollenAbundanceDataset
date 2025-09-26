# Select Optimal Number of Trees 
library(optRF)
set.seed(123)
result <- opt_prediction(
  y = y_train,
  X = x_train,
  num.trees_values = c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,
                       1400,1500,1600,1700,1800,1900,2000,2100,
                       2200,2300,2400,2500,2600,2700,2800,2900,3000),
  number_repetitions = 20,
  visualisation = "prediction",
  recommendation = "prediction"
)
summary(result)

# Tune Optimal Hyperparameters 
library(tuneRanger)
library(mlr)
# Convert data type and remove unnecessary columns: longitude, latitude, and reference
train_set <- train_set[,-c(1,2,4)]
test_set <- test_set[,-c(1,2,4)]
train_set <- as.data.frame(train_set)
# Create task
set.seed(123)
task <- makeRegrTask(data = train_set, target = "pa")
res <- tuneRanger(task, iters = 500, iters.warmup = 200, num.trees = 1000)
res$model