HW5
================
Jiayu Shi

``` r
# install.packages("xgboost")
# install.packages("caret")
```

# Xgboost

``` r
library(xgboost)
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

## Load the data and preprocess it:

``` r
# Load the data
train_data <- read.table("vowel.train.txt", header = TRUE, sep = ",")
test_data <- read.table("vowel.test.txt", header = TRUE, sep = ",")

# Split the features and labels
train_features <- train_data[, 3:ncol(train_data)]
train_labels <- train_data[, 2]

test_features <- test_data[, 3:ncol(test_data)]
test_labels <- test_data[, 2]

# Convert to xgb.DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(train_features), label = train_labels - 1)
# to convert the class labels to a zero-based indexing system. XGBoost expects the class labels to start from 0, 
dtest <- xgb.DMatrix(data = as.matrix(test_features), label = test_labels - 1)
```

## Fit a gradient boosted model to the “vowel.train” data using all of the 10 features with default values of the tuning parameters:

``` r
params <- list(
  objective = "multi:softprob",
  num_class = length(unique(train_labels)),
  eval_metric = "merror"
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 10,
  watchlist = list(train = dtrain),
  print_every_n = 1
)
```

    ## [1]  train-merror:0.073864 
    ## [2]  train-merror:0.035985 
    ## [3]  train-merror:0.020833 
    ## [4]  train-merror:0.013258 
    ## [5]  train-merror:0.009470 
    ## [6]  train-merror:0.007576 
    ## [7]  train-merror:0.005682 
    ## [8]  train-merror:0.001894 
    ## [9]  train-merror:0.001894 
    ## [10] train-merror:0.000000

## Use 5-fold CV to tune the ensemble size:

``` r
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 20,
  nfold = 5,
  print_every_n = 1,
  early_stopping_rounds = 1,
  maximize = FALSE
)
```

    ## [1]  train-merror:0.100874+0.011604  test-merror:0.316020+0.066422 
    ## Multiple eval metrics are present. Will use test_merror for early stopping.
    ## Will train until test_merror hasn't improved in 1 rounds.
    ## 
    ## [2]  train-merror:0.040264+0.008119  test-merror:0.251566+0.053517 
    ## [3]  train-merror:0.017057+0.005509  test-merror:0.206113+0.040603 
    ## [4]  train-merror:0.010903+0.003580  test-merror:0.200361+0.032277 
    ## [5]  train-merror:0.007584+0.003807  test-merror:0.189073+0.036378 
    ## [6]  train-merror:0.004741+0.002133  test-merror:0.179710+0.021354 
    ## [7]  train-merror:0.003319+0.001174  test-merror:0.175791+0.028897 
    ## [8]  train-merror:0.001424+0.001903  test-merror:0.168367+0.029672 
    ## [9]  train-merror:0.000948+0.001161  test-merror:0.162829+0.034268 
    ## [10] train-merror:0.000000+0.000000  test-merror:0.160833+0.036226 
    ## [11] train-merror:0.000000+0.000000  test-merror:0.151432+0.038825 
    ## [12] train-merror:0.000000+0.000000  test-merror:0.147604+0.035622 
    ## [13] train-merror:0.000000+0.000000  test-merror:0.145681+0.037519 
    ## [14] train-merror:0.000000+0.000000  test-merror:0.141888+0.040475 
    ## [15] train-merror:0.000000+0.000000  test-merror:0.145734+0.036682 
    ## Stopping. Best iteration:
    ## [14] train-merror:0.000000+0.000000  test-merror:0.141888+0.040475

``` r
# Optimal number of rounds
optimal_nrounds <- cv_results$best_iteration
print(optimal_nrounds)
```

    ## [1] 14

## With the tuned model, make predictions using the majority vote method, and compute the misclassification rate using the vowel.test data:

``` r
# Train the model with the optimal number of rounds
xgb_tuned_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = optimal_nrounds,
  watchlist = list(train = dtrain),
  print_every_n = 1
)
```

    ## [1]  train-merror:0.073864 
    ## [2]  train-merror:0.035985 
    ## [3]  train-merror:0.020833 
    ## [4]  train-merror:0.013258 
    ## [5]  train-merror:0.009470 
    ## [6]  train-merror:0.007576 
    ## [7]  train-merror:0.005682 
    ## [8]  train-merror:0.001894 
    ## [9]  train-merror:0.001894 
    ## [10] train-merror:0.000000 
    ## [11] train-merror:0.000000 
    ## [12] train-merror:0.000000 
    ## [13] train-merror:0.000000 
    ## [14] train-merror:0.000000

``` r
# Make predictions
test_pred_probs <- predict(xgb_tuned_model, dtest)
test_pred <- matrix(test_pred_probs, ncol = length(unique(train_labels)), byrow = TRUE)
test_pred_class <- max.col(test_pred)

# Compute the misclassification rate
misclassification_rate <- mean(test_pred_class != (test_labels))
cat("Misclassification rate:", misclassification_rate, "\n")
```

    ## Misclassification rate: 0.491342

# Random Forest

``` r
# install.packages("randomForest")
```

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

## Fit a random forest model to the “vowel.train” data using all of the 11 features with default values of the tuning parameters:

``` r
rf_model <- randomForest(x = train_features, y = train_labels)
```

## Use 5-fold CV to tune the number of variables randomly sampled as candidates at each split:

``` r
# Set up cross-validation
cv_control <- trainControl(method = "cv", number = 5)

# Tune the model
tune_grid <- expand.grid(.mtry = seq(1, ncol(train_features), 1)) # Try all values from 1 to the number of features

tuned_rf_model <- train(
  x = train_features,
  y = train_labels,
  method = "rf",
  trControl = cv_control,
  tuneGrid = tune_grid
)

# Optimal mtry
optimal_mtry <- tuned_rf_model$bestTune$mtry
print(optimal_mtry)
```

    ## [1] 4

## With the tuned model, make predictions using the majority vote method, and compute the misclassification rate using the vowel.test data:

``` r
# Train the random forest model with the optimal mtry
rf_tuned_model <- randomForest(x = train_features, y = train_labels, mtry = optimal_mtry)

# Make predictions
test_pred_class <- predict(rf_tuned_model, test_features)

# Compute the misclassification rate
misclassification_rate <- mean(test_pred_class != test_labels)
cat("Misclassification rate:", misclassification_rate, "\n")
```

    ## Misclassification rate: 1
