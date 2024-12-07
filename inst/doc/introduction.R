## ----eval = FALSE-------------------------------------------------------------
#  library(SA24204158)
#  help(knnC)

## -----------------------------------------------------------------------------
library(Rcpp)
library(SA24204158)
set.seed(123)
# Sample training and test data
train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
train_labels <- c(0, 0, 1, 1)
test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)

# Set the number of neighbors
k <- 3

# Run KNN benchmark
benchmark_results <- benchmark_knn(train_data, train_labels, test_data, k)
print(benchmark_results)

## -----------------------------------------------------------------------------
# Sample training and test data
train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
train_labels <- c(0, 0, 1, 1)
test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)

# KNN, DNN, and TDNN predictions
predictions_knn <- knnC(train_data, train_labels, test_data, k = 3)
predictions_dnn <- dnnC(train_data, train_labels, test_data, s = 3)
predictions_tdnn <- tdnnC(train_data, train_labels, test_data, s1 = 1, s2 = 2)

# Print predictions
print(predictions_knn)
print(predictions_dnn)
print(predictions_tdnn)

## -----------------------------------------------------------------------------
set.seed(123)
# Generate synthetic data
train_data <- matrix(rnorm(200 * 2), ncol = 2)
train_labels <- sample(c(0, 1), size = 200, replace = TRUE)

# Cross-validation for KNN (find best k)
k_values <- 1:10
best_k <- knnCvC(train_data, train_labels, k_values)
print(best_k)

# Cross-validation for DNN (find best s)
s_values <- 1:10
best_s <- dnnCvC(train_data, train_labels, s_values)
print(best_s)

# Cross-validation for TDNN (find best s2)
s2_values <- 1:10
best_s2 <- tdnnCvC(train_data, train_labels, s2_values , c=2)
print(best_s2)

## ----echo = FALSE-------------------------------------------------------------
rm(list = ls())

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(123)
#  # Set parameters
#  d <- 10         # Feature dimension
#  n <- 1000      # Number of data points
#  n_experiments <- 100  # Number of Monte Carlo experiments
#  
#  # Load necessary libraries
#  library(MASS)       # For mvrnorm
#  library(extraDistr) # For rlaplace
#  
#  # Function to generate data
#  generate_data <- function(n, d) {
#    # Generate data for class 1 (Laplace distribution)
#    f1 <- matrix(rlaplace(n * d, mu = 0, sigma = 1), nrow = n, ncol = d)
#    # Generate data for class 2 (Multivariate normal distribution)
#    f2 <- mvrnorm(n, mu = rep(1, d), Sigma = diag(d))
#  
#    # Combine data
#    data <- rbind(f1, f2)
#  
#    # Assign labels: first n samples are class 1, next n samples are class 2
#    labels <- c(rep(1, n), rep(0, n))
#  
#    return(list(data = data, labels = labels))
#  }
#  
#  dmvnorm_custom <- function(x, mean, sigma) {
#    d <- length(mean)
#    diff <- x - mean
#    exponent <- -0.5 * t(diff) %*% solve(sigma) %*% diff
#    normalization <- 1 / sqrt((2 * pi)^d * det(sigma))
#    return(normalization * exp(exponent))
#  }
#  
#  
#  # Function to calculate eta(x)
#  calculate_eta <- function(x) {
#    # Probability density for class 1 (Laplace distribution)
#    p1 <- exp(-sum(abs(x))) / (2^d)  # Adjust Laplace PDF formula
#  
#    # Probability density for class 2 (Multivariate normal distribution)
#     p2 <- dmvnorm_custom(x, mean = rep(1, d), sigma = diag(d)) # Use custom dmvnorm
#  
#    # Calculate eta(x)
#    eta <- p1 / (p1 + p2)
#  
#    return(eta)
#  }
#  
#  # Function to compute Bayes risk
#  bayes_risk <- function(data, labels) {
#    predicted_class <- apply(data, 1, function(x) {
#      eta <- calculate_eta(x)
#      ifelse(eta > 0.5, 1, 0)
#    })
#  
#    # Calculate the error rate
#    risk <- mean(predicted_class != labels)
#    return(risk)
#  }
#  
#  # Run Monte Carlo experiments
#  total_risk <- 0
#  for (i in 1:n_experiments) {
#    # Generate data
#    data_info <- generate_data(n, d)
#    data <- data_info$data
#    labels <- data_info$labels
#  
#    # Compute Bayes risk
#    risk <- bayes_risk(data, labels)
#  
#    # Accumulate the risk
#    total_risk <- total_risk + risk
#  }
#  
#  # Calculate and print the average risk
#  average_risk <- total_risk / n_experiments
#  print(paste("Average Bayes Risk:", average_risk))
#  

## ----echo = FALSE-------------------------------------------------------------
rm(list = ls()) 

## ----eval = FALSE-------------------------------------------------------------
#  library(Rcpp)
#  library(SA24204158)
#  library(MASS)
#  library(extraDistr)
#  set.seed(123)
#  # Function to run a single experiment and compute accuracy and time
#  run_experiment <- function(d, n_train, n_test) {
#    # Record start time
#    start_time <- Sys.time()
#  
#    # Generate training and test sets
#    train_f1 <- matrix(rlaplace(n_train * d, mu = 0, sigma = 1), nrow = n_train, ncol = d)
#    train_f2 <- mvrnorm(n_train, mu = rep(1, d), Sigma = diag(d))
#    test_f1 <- matrix(rlaplace(n_test * d, mu = 0, sigma = 1), nrow = n_test, ncol = d)
#    test_f2 <- mvrnorm(n_test, mu = rep(1, d), Sigma = diag(d))
#  
#    # Combine training and test sets
#    train_data <- rbind(train_f1, train_f2)
#    test_data <- rbind(test_f1, test_f2)
#  
#    # Assign labels: first n_train samples are class 0, next n_train samples are class 1
#    train_labels <- c(rep(0, n_train), rep(1, n_train))
#    test_labels <- c(rep(0, n_test), rep(1, n_test))
#  
#    # Use CV to select parameters for DNN and TDNN
#    s_values <- 1:10
#    best_s <- floor((5/4)**(d/(d+4))*dnnCvC(train_data, train_labels, s_values))
#    s2_values <- 1:10
#    best_s2 <- floor((5/4)**(d/(d+8))*tdnnCvC(train_data, train_labels, s2_values , c=2))
#  
#    # Predict using DNN
#    dnn_predictions <- dnnC(train_data, train_labels, test_data, best_s)
#  
#    # Predict using TDNN
#    s1 <- 2 * best_s2
#    tdnn_predictions <- tdnnC(train_data, train_labels, test_data, s1, best_s2)
#  
#    # Compute accuracies
#    dnn_accuracy <- mean(dnn_predictions == test_labels)
#    tdnn_accuracy <- mean(tdnn_predictions == test_labels)
#  
#    # Record end time and compute elapsed time
#    end_time <- Sys.time()
#    elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
#  
#    return(list(dnn_accuracy = dnn_accuracy, tdnn_accuracy = tdnn_accuracy, elapsed_time = elapsed_time))
#  }
#  
#  # Set experiment parameters
#  d <- 1  # Feature dimension
#  n_train <- 1000   #Number of training samples
#  n_test <- 200   # Number of test samples
#  n_iterations <- 100  # Number of Monte Carlo experiments
#  
#  # Store accuracies and times for each experiment
#  dnn_accuracies <- numeric(n_iterations)
#  tdnn_accuracies <- numeric(n_iterations)
#  times <- numeric(n_iterations)
#  
#  # Run Monte Carlo experiments
#  for (i in 1:n_iterations) {
#    results <- run_experiment(d, n_train, n_test)
#    dnn_accuracies[i] <- results$dnn_accuracy
#    tdnn_accuracies[i] <- results$tdnn_accuracy
#    times[i] <- results$elapsed_time
#  }
#  
#  # Compute average accuracies
#  mean_dnn_accuracy <- mean(dnn_accuracies)
#  mean_tdnn_accuracy <- mean(tdnn_accuracies)
#  
#  # Compute average elapsed time
#  mean_time <- mean(times)
#  
#  # Output results
#  cat("Average DNN Accuracy: ", mean_dnn_accuracy, "\n")
#  cat("Average TDNN Accuracy: ", mean_tdnn_accuracy, "\n")
#  cat("Average DNN Risk: ", 1-mean_dnn_accuracy, "\n")
#  cat("Average TDNN Risk: ", 1-mean_tdnn_accuracy, "\n")
#  cat("Average Time per Experiment: ", mean_time, "seconds\n")

