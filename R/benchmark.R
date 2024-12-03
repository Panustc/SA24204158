#' @title Benchmark KNN Functions (R vs C)
#' @description Use the \code{microbenchmark} package to compare the performance of the C function \code{knnC} and the R function \code{knnR}.
#' @param train_data A numeric matrix where rows are training samples and columns are features.
#' @param train_labels An integer vector representing the labels for the training data.
#' @param test_data A numeric matrix where rows are test samples and columns are features.
#' @param k An integer specifying the number of nearest neighbors to consider.
#' @return A \code{microbenchmark} object that contains the execution time of both functions.
#' @examples
#' \dontrun{
#' # Example data
#' train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
#' train_labels <- c(0, 0, 1, 1)
#' test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)
#' k <- 3
#' # Run the benchmark
#' benchmark_results <- benchmark_knn(train_data, train_labels, test_data, k)
#' print(benchmark_results)
#' }
#' @export
#' @import microbenchmark Rcpp
benchmark_knn <- function(train_data, train_labels, test_data, k) {
  # Ensure the microbenchmark package is loaded
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    stop("The microbenchmark package is required but not installed.")
  }
  
  # Run the benchmark using microbenchmark for knnC and knnR
  benchmark_results <- microbenchmark(
    knnC(train_data, train_labels, test_data, k),
    knnR(train_data, train_labels, test_data, k),
    times = 100L  # Number of times to repeat the benchmark
  )
  
  return(benchmark_results)
}
