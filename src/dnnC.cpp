#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <map>
using namespace Rcpp;

double calc_Dn(const NumericVector& x, const NumericMatrix& train_data, 
                const IntegerVector& train_labels, int s, int n_features) {
   int n_train = train_data.nrow();
   std::vector<std::pair<double, int>> distances;
   for (int i = 0; i < n_train; ++i) {
     double distance = 0.0;
     for (int f = 0; f < n_features; ++f) {
       distance += std::pow(x[f] - train_data(i, f), 2);
     }
     distance = std::sqrt(distance);
     distances.push_back(std::make_pair(distance, train_labels[i]));
   }
   
   std::sort(distances.begin(), distances.end());
   double weighted_sum = 0.0;
   for (int i = 0; i < (n_train - s + 1); ++i) {
     double weight = R::choose(n_train - i, s - 1); 
     weighted_sum += weight * distances[i].second;  
   }
   double Dn = weighted_sum / R::choose(n_train, s);  
   return Dn;
 }

//' @title DNN Classifier
//' @description A DNN implementation using Rcpp
//' @param train_data A numeric matrix where rows are training samples and columns are features
//' @param train_labels An integer vector representing labels for the training data
//' @param test_data A numeric matrix where rows are test samples and columns are features
//' @param s An integer specifying the number of nearest neighbors to consider for the weighted sum
//' @return An integer vector containing the predicted labels for the test samples
//' @examples
//' \dontrun{
//' train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
//' train_labels <- c(0, 0, 1, 1)
//' test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)
//' predictions <- dnnC(train_data, train_labels, test_data, s = 3)
//' print(predictions)
//' }
//' @export
//' @useDynLib SA24204158, .registration = TRUE  
// [[Rcpp::export]]
IntegerVector dnnC(const NumericMatrix& train_data, 
                   const IntegerVector& train_labels, 
                   const NumericMatrix& test_data, 
                   int s) {
  int n_train = train_data.nrow();
  int n_test = test_data.nrow();
  int n_features = train_data.ncol();
  IntegerVector predictions(n_test);
  
  // For each test sample, compute D_n(s)(x) and classify based on the decision rule
  for (int i = 0; i < n_test; ++i) {
    NumericVector test_sample = test_data(i, _);  // Get the current test sample
    
    // Compute D_n(s)(x) for the current test sample
    double Dn_value = calc_Dn(test_sample, train_data, train_labels, s, n_features);
    
    // Apply the decision rule based on the value of D_n(s)(x)
    if (Dn_value >= 0.5) {
      predictions[i] = 1;  // If D_n(s)(x) >= 0.5, predict label 1
    } else {
      predictions[i] = 0;  // Otherwise, predict label 0
    }
  }
  
  // Return the predicted labels for all test samples
  return predictions;
}



//' @title DNN Classifier with Cross-Validation
//' @description A DNN implementation that selects the best s using 5-fold cross-validation
//' @param train_data A numeric matrix where rows are training samples and columns are features
//' @param train_labels An integer vector representing labels for the training data
//' @param s_values A vector of integers representing the candidate values of s for cross-validation
//' @return The optimal s value that maximizes the average cross-validation accuracy
//' @examples
//' \dontrun{
//' train_data <- matrix(rnorm(200 * 2), ncol = 2)
//' train_labels <- sample(c(0, 1), size = 200, replace = TRUE)
//' s_values <- c(1, 2, 3, 4, 5)
//' best_s <- dnnCvC(train_data, train_labels, s_values)
//' print(best_s)
//' }
//' @export
//' @useDynLib SA24204158, .registration = TRUE 
// [[Rcpp::export]]
int dnnCvC(const NumericMatrix& train_data, const IntegerVector& train_labels, 
           const IntegerVector& s_values) {
  
  int n_samples = train_data.nrow();
  int n_folds = 5;
  int n_s_values = s_values.size();
  
  // Vector to store cross-validation accuracy for each s value
  std::vector<double> s_accuracies(n_s_values, 0.0);
  
  // Perform 5-fold cross-validation
  for (int s_idx = 0; s_idx < n_s_values; ++s_idx) {
    int s = s_values[s_idx];
    
    // Initialize accuracy for this s value
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
        
        IntegerVector predictions = dnnC(train_fold, train_labels_fold, test_sample, s);
        if (predictions[0] == test_labels_fold[i]) {
          correct_predictions++;
        }
      }
      
      // Calculate accuracy for this fold
      double accuracy = static_cast<double>(correct_predictions) / test_fold.nrow();
      total_accuracy += accuracy;
    }
    
    // Store average accuracy for this s value
    s_accuracies[s_idx] = total_accuracy / n_folds;
  }
  
  // Find the s value that gives the highest accuracy
  int best_s_index = std::distance(s_accuracies.begin(), std::max_element(s_accuracies.begin(), s_accuracies.end()));
  return s_values[best_s_index];
}


//' @title TDNN Classifier
//' @description A TDNN implementation using Rcpp
//' @param train_data A numeric matrix where rows are training samples and columns are features
//' @param train_labels An integer vector representing labels for the training data
//' @param test_data A numeric matrix where rows are test samples and columns are features
//' @param s1 An integer specifying the first subsample size
//' @param s2 An integer specifying the second subsample size
//' @return An integer vector containing the predicted labels for the test samples
//' @examples
//' \dontrun{
//' train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
//' train_labels <- c(0, 0, 1, 1)
//' test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)
//' predictions <- tdnnC(train_data, train_labels, test_data, s1 = 3, s2 = 5)
//' print(predictions)
//' }
//' @export
//' @useDynLib SA24204158, .registration = TRUE  
// [[Rcpp::export]]
IntegerVector tdnnC(const NumericMatrix& train_data, 
                     const IntegerVector& train_labels, 
                     const NumericMatrix& test_data, 
                     int s1, int s2) {
   
   int n_train = train_data.nrow();
   int n_test = test_data.nrow();
   int n_features = train_data.ncol();
   IntegerVector predictions(n_test);
   
   // Calculate the weights w1* and w2*
   double ratio = std::pow(s1 / double(s2), -2.0 / double(n_features));
   double w1_star = 1.0 / (1.0 - ratio);
   double w2_star = -ratio / (1.0 - ratio);
   
   // For each test sample, calculate D_n(s1, s2)(x)
   for (int i = 0; i < n_test; ++i) {
     NumericVector test_sample = test_data(i, _);  // 当前测试样本
     
     // calculate D_n(s1)(x) and D_n(s2)(x)
     double Dn_s1 = calc_Dn(test_sample, train_data, train_labels, s1, n_features);
     double Dn_s2 = calc_Dn(test_sample, train_data, train_labels, s2, n_features);
     
     // Calculation TDNN estimate D_n(s1, s2)(x)
     double Dn_combined = w1_star * Dn_s1 + w2_star * Dn_s2;
     
     if (Dn_combined >= 0.5) {
       predictions[i] = 1;  
     } else {
       predictions[i] = 0;  
     }
   }

   return predictions;
 }


//' @title TDNN Classifier with Cross-Validation for s2
//' @description A TDNN implementation that selects the best s2 using 5-fold cross-validation, with s1 = 2 * s2
//' @param train_data A numeric matrix where rows are training samples and columns are features
//' @param train_labels An integer vector representing labels for the training data
//' @param s2_values A vector of integers representing the candidate values of s2 for cross-validation
//' @param c A scalar parameter that defines the relationship between s1 and s2. Specifically, s1 = c * s2. 
//' @return The optimal s2 value that maximizes the average cross-validation accuracy
//' @examples
//' \dontrun{
//' train_data <- matrix(rnorm(200 * 2), ncol = 2)
//' train_labels <- sample(c(0, 1), size = 200, replace = TRUE)
//' s2_values <- c(1, 2, 3, 4, 5)
//' best_s2 <- tdnnCvC(train_data, train_labels, s2_values)
//' print(best_s2)
//' }
//' @export
//' @useDynLib SA24204158, .registration = TRUE 
// [[Rcpp::export]]
int tdnnCvC(const NumericMatrix& train_data, const IntegerVector& train_labels, 
             const IntegerVector& s2_values, const double c) {
   
   int n_samples = train_data.nrow();
   int n_folds = 5;
   int n_s2_values = s2_values.size();
   
   // Vector to store cross-validation accuracy for each s2 value
   std::vector<double> s2_accuracies(n_s2_values, 0.0);
   
   // Perform 5-fold cross-validation
   for (int s2_idx = 0; s2_idx < n_s2_values; ++s2_idx) {
     int s2 = s2_values[s2_idx];
     int s1 = static_cast<int>(c * s2);  // Set s1 = c * s2
     
     // Initialize accuracy for this s2 value
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
         NumericMatrix test_sample(1, test_fold.ncol());  // Create a 1-row matrix with the same number of columns as test_fold
         for (int j = 0; j < test_fold.ncol(); ++j) {
           test_sample(0, j) = test_fold(i, j);  // Copy the values from the ith row of test_fold to test_sample
         }
         
         IntegerVector predictions = tdnnC(train_fold, train_labels_fold, test_sample, s1, s2);
         if (predictions[0] == test_labels_fold[i]) {
           correct_predictions++;
         }
       }
       
       // Calculate accuracy for this fold
       double accuracy = static_cast<double>(correct_predictions) / test_fold.nrow();
       total_accuracy += accuracy;
     }
     
     // Store average accuracy for this s2 value
     s2_accuracies[s2_idx] = total_accuracy / n_folds;
   }
   
   // Find the s2 value that gives the highest accuracy
   int best_s2_index = std::distance(s2_accuracies.begin(), 
                                     std::max_element(s2_accuracies.begin(), s2_accuracies.end()));
   return s2_values[best_s2_index];
 }
