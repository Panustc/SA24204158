#' @title DNN Classifier (R version)
#' @description A DNN implementation using R
#' @param train_data A numeric matrix where rows are training samples and columns are features
#' @param train_labels An integer vector representing labels for the training data
#' @param test_data A numeric matrix where rows are test samples and columns are features
#' @param s An integer specifying the number of nearest neighbors to consider for the weighted sum
#' @return An integer vector containing the predicted labels for the test samples
#' @examples
#' \dontrun{
#' train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
#' train_labels <- c(0, 0, 1, 1)
#' test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)
#' predictions <- dnnR(train_data, train_labels, test_data, s = 3)
#' print(predictions)
#' }
#' @export
dnnR <- function(train_data, train_labels, test_data, s) {
  # Number of training and test samples
  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  
  # Placeholder for predictions
  predictions <- integer(n_test)
  
  # Function to calculate the D_n(s)(x) estimator
  calc_Dn <- function(x, train_data, train_labels, s) {
    # Calculate Euclidean distances between test point and all training samples
    distances <- apply(train_data, 1, function(y) sqrt(sum((y - x)^2)))
    
    # Order the distances and get the corresponding labels
    ordered_labels <- train_labels[order(distances)]
    
    # Calculate the weighted sum (D_n(s)(x)) using binomial coefficients
    weighted_sum <- 0
    for (i in 1:(n_train - s + 1)) {
      weight <- choose(n_train - i, s - 1)
      weighted_sum <- weighted_sum + weight * ordered_labels[i]
    }
    
    # Normalize by the total number of combinations
    Dn <- weighted_sum / choose(n_train, s)
    
    return(Dn)
  }
  
  # For each test sample
  for (i in 1:n_test) {
    test_sample <- test_data[i, ]
    
    # Calculate D_n(s)(x) for the current test sample
    Dn_value <- calc_Dn(test_sample, train_data, train_labels, s)
    
    # Apply the decision rule
    if (Dn_value >= 0.5) {
      predictions[i] <- 1
    } else {
      predictions[i] <- 0
    }
  }
  
  return(predictions)
}
