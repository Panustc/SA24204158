#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <map>
#include <numeric>
using namespace Rcpp;

//' @title K-Nearest Neighbors Classifier
//' @description A simple KNN implementation using Rcpp
//' @param train_data A numeric matrix where rows are training samples and columns are features
//' @param train_labels An integer vector representing labels for the training data
//' @param test_data A numeric matrix where rows are test samples and columns are features
//' @param k An integer specifying the number of nearest neighbors to consider
//' @return An integer vector containing the predicted labels for the test samples
//' @examples
//' \dontrun{
//' train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
//' train_labels <- c(0, 0, 1, 1)
//' test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)
//' predictions <- knnC(train_data, train_labels, test_data, k = 3) 
//' print(predictions)
//' }
//' @export
//' @useDynLib SA24204158, .registration = TRUE  
// [[Rcpp::export]]
IntegerVector knnC(const NumericMatrix& train_data, 
                   const IntegerVector& train_labels, 
                   const NumericMatrix& test_data, 
                   int k) {
  int n_train = train_data.nrow();
  int n_test = test_data.nrow();
  int n_features = train_data.ncol();
  IntegerVector predictions(n_test);
  
  for (int i = 0; i < n_test; ++i) {
    std::vector<std::pair<double, int>> distances;
    
    for (int j = 0; j < n_train; ++j) {
      double distance = 0.0;
      for (int f = 0; f < n_features; ++f) {
        distance += std::pow(test_data(i, f) - train_data(j, f), 2);
      }
      distance = std::sqrt(distance);
      distances.push_back(std::make_pair(distance, train_labels[j]));
    }
    
    std::sort(distances.begin(), distances.end());
    std::map<int, int> label_count;
    for (int k_i = 0; k_i < k; ++k_i) {
      label_count[distances[k_i].second]++;
    }
    
    int max_count = 0, predicted_label = -1;
    for (const auto& pair : label_count) {
      if (pair.second > max_count) {
        max_count = pair.second;
        predicted_label = pair.first;
      }
    }
    predictions[i] = predicted_label;
  }
  return predictions;
}

//' @title K-Nearest Neighbors Classifier with Cross-Validation
//' @description A KNN implementation that selects the best K using 5-fold cross-validation
//' @param train_data A numeric matrix where rows are training samples and columns are features
//' @param train_labels An integer vector representing labels for the training data
//' @param k_values A vector of integers representing the candidate values of k for cross-validation
//' @return The optimal k value that maximizes the average cross-validation accuracy
//' @examples
//' \dontrun{
//' train_data <- matrix(rnorm(200 * 2), ncol = 2)
//' train_labels <- sample(c(0, 1), size = 200, replace = TRUE)
//' k_values <- c(1, 2,3,4,5)
//' best_k <- knnCvC(train_data, train_labels, k_values)
//' print(best_k)
//' }
//' @export
//' @useDynLib SA24204158, .registration = TRUE 
// [[Rcpp::export]]
int knnCvC(const NumericMatrix& train_data, const IntegerVector& train_labels, 
            const IntegerVector& k_values) {
   
   int n_samples = train_data.nrow();
   int n_folds = 5;
   int n_k_values = k_values.size();
   
   // Vector to store cross-validation accuracy for each k value
   std::vector<double> k_accuracies(n_k_values, 0.0);
   
   // Perform 5-fold cross-validation
   for (int k_idx = 0; k_idx < n_k_values; ++k_idx) {
     int k = k_values[k_idx];
     
     // Initialize accuracy for this k value
     double total_accuracy = 0.0;
     
     // Split data into 5 folds
     for (int fold = 0; fold < n_folds; ++fold) {
       // Define the start and end indices for the fold
       int fold_size = n_samples / n_folds;
       int fold_start = fold * fold_size;
       int fold_end = (fold == n_folds - 1) ? n_samples : (fold + 1) * fold_size;
       
       // Create training and test sets for this fold
       IntegerVector test_indices = seq(fold_start, fold_end - 1);
       IntegerVector train_indices;
       
       for (int i = 0; i < n_samples; ++i) {
         if (std::find(test_indices.begin(), test_indices.end(), i) == test_indices.end()) {
           train_indices.push_back(i);
         }
       }
       
       // Create training and testing subsets
       NumericMatrix train_fold(train_indices.size(), train_data.ncol());
       IntegerVector train_labels_fold(train_indices.size());
       NumericMatrix test_fold(test_indices.size(), train_data.ncol());
       IntegerVector test_labels_fold(test_indices.size());
       
       for (int i = 0; i < train_indices.size(); ++i) {
         for (int j = 0; j < train_data.ncol(); ++j) {
           train_fold(i, j) = train_data(train_indices[i], j); 
         }
         train_labels_fold[i] = train_labels[train_indices[i]];
       }
       for (int i = 0; i < test_indices.size(); ++i) {
         for (int j = 0; j < train_data.ncol(); ++j) {
           test_fold(i, j) = train_data(test_indices[i], j);
         }
         test_labels_fold[i] = train_labels[test_indices[i]];
       }
       
       // Perform predictions for the test fold
       int correct_predictions = 0;
       for (int i = 0; i < test_fold.nrow(); ++i) {
         // Predict label for test sample
         // Perform predictions for the test fold
         NumericMatrix test_sample(1, test_fold.ncol());  // Create a 1-row matrix with the same number of columns as test_fold
         for (int j = 0; j < test_fold.ncol(); ++j) {
           test_sample(0, j) = test_fold(i, j);  // Copy the values from the ith row of test_fold to test_sample
         }
         
         IntegerVector predictions = knnC(train_fold, train_labels_fold, test_sample, k);
         if (predictions[0] == test_labels_fold[i]) {
           correct_predictions++;
         }
       }
       
       // Calculate accuracy for this fold
       double accuracy = static_cast<double>(correct_predictions) / test_fold.nrow();
       total_accuracy += accuracy;
     }
     
     // Store average accuracy for this k value
     k_accuracies[k_idx] = total_accuracy / n_folds;
   }
   
   // Find the k value that gives the highest accuracy
   int best_k_index = std::distance(k_accuracies.begin(), std::max_element(k_accuracies.begin(), k_accuracies.end()));
   return k_values[best_k_index];
 }

