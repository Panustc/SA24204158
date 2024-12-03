#' @title K-Nearest Neighbors Classifier (R version)
#' @description A simple KNN implementation using R
#' @param train_data A numeric matrix where rows are training samples and columns are features
#' @param train_labels An integer vector representing labels for the training data
#' @param test_data A numeric matrix where rows are test samples and columns are features
#' @param k An integer specifying the number of nearest neighbors to consider
#' @return An integer vector containing the predicted labels for the test samples
#' @examples
#' \dontrun{
#' train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
#' train_labels <- c(0, 0, 1, 1)
#' test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)
#' predictions <- knnR(train_data, train_labels, test_data, k = 3)
#' print(predictions)
#' }
#' @export
knnR <- function(train_data, train_labels, test_data, k) {
  # Number of training and test samples
  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  
  # Placeholder for predictions
  predictions <- integer(n_test)
  
  # For each test sample
  for (i in 1:n_test) {
    # Calculate distances from the current test sample to all training samples
    distances <- apply(train_data, 1, function(x) sqrt(sum((x - test_data[i, ])^2)))
    
    # Get the indices of the k nearest neighbors
    nearest_neighbors <- order(distances)[1:k]
    
    # Get the labels of the nearest neighbors
    neighbor_labels <- train_labels[nearest_neighbors]
    
    # Find the most common label among the nearest neighbors (majority voting)
    predictions[i] <- as.integer(names(sort(table(neighbor_labels), decreasing = TRUE))[1])
  }
  
  return(predictions)
}
