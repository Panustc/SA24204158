% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{tdnnC}
\alias{tdnnC}
\title{TDNN Classifier}
\usage{
tdnnC(train_data, train_labels, test_data, s1, s2)
}
\arguments{
\item{train_data}{A numeric matrix where rows are training samples and columns are features}

\item{train_labels}{An integer vector representing labels for the training data}

\item{test_data}{A numeric matrix where rows are test samples and columns are features}

\item{s1}{An integer specifying the first subsample size}

\item{s2}{An integer specifying the second subsample size}
}
\value{
An integer vector containing the predicted labels for the test samples
}
\description{
A TDNN implementation using Rcpp
}
\examples{
\dontrun{
train_data <- matrix(c(1, 2, 1, 3, 4, 5, 6, 7), ncol = 2)
train_labels <- c(0, 0, 1, 1)
test_data <- matrix(c(1.5, 2.5, 6, 7), ncol = 2)
predictions <- tdnnC(train_data, train_labels, test_data, s1 = 3, s2 = 5)
print(predictions)
}
}
