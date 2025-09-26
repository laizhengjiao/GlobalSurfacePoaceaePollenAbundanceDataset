library(VSURF)
library(caret)
library(parallel)
library(doParallel)

# Set up parallel computing (5 cores)
num_cores <- 5
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Data preparation
response_var <- "pa"
predictors <- names(train_set)[5:ncol(train_set)]
x_train <- train_set[, predictors]
y_train <- train_set[[response_var]]

set.seed(123)
# Execute VSURF
vsurf_result <- VSURF(
  x = x_train,
  y = y_train,
  mtry = max(floor(ncol(x_train)/3), 1),
  ntree.thres = 2000,
  nfor.thres = 50,
  nmin = 1,
  ntree.interp = 2000,
  nfor.interp = 50,
  nsd = 1,
  ntree.pred = 2000,
  nfor.pred = 50,
  nmj = 1,
  RFimplem = "ranger",
  verbose = TRUE,
  parallel = TRUE
)

# End parallel computing
stopCluster(cl)   # Stop the cluster
registerDoParallel()  # Clear registration

# Output results
summary(vsurf_result)

# Save visualization
pdf("yourpath/alldata_vsurf_plot.pdf", width = 25, height = 20)
plot(vsurf_result, var.names = TRUE)
dev.off()
