#include <Rcpp.h>
using namespace Rcpp;

//' @title Gibbs Sampler for a Binomial-Beta Model
//' @description This function performs Gibbs sampling for a Binomial-Beta hierarchical model. 
//' It generates samples from the joint posterior distribution of \code{x} and \code{y}.
//' @param n_iter An integer specifying the number of iterations for the Gibbs sampler.
//' @param n An integer representing the total number of trials for the Binomial distribution.
//' @param a A numeric value specifying the shape parameter \code{a} of the Beta prior.
//' @param b A numeric value specifying the shape parameter \code{b} of the Beta prior.
//' @return A numeric matrix of size \code{n_iter} x 2 where each row contains a sample of \code{x} (column 1) and \code{y} (column 2).
//' @details 
//' The Gibbs sampler alternates between two steps:
//' \itemize{
//'   \item Sampling \code{x} given \code{y} from a Binomial distribution with parameters \code{n} and \code{y}.
//'   \item Sampling \code{y} given \code{x} from a Beta distribution with parameters \code{x + a} and \code{n - x + b}.
//' }
//' This procedure approximates the joint posterior distribution of \code{x} and \code{y}.
//' @examples
//' \dontrun{
//' n_iter <- 1000
//' n <- 10
//' a <- 2
//' b <- 2
//' samples <- gibbs_sampler(n_iter, n, a, b)
//' plot(samples[,1], samples[,2], main = "Gibbs Sampler", xlab = "x", ylab = "y")
//' }
//' @export
//' @useDynLib SA24204158, .registration = TRUE
// [[Rcpp::export]]
NumericMatrix gibbs_sampler(int n_iter, int n, double a, double b) {
  NumericMatrix samples(n_iter, 2); // 存储样本的矩阵
  int x = 0;                        // 初始化 x
  double y = 0.5;                   // 初始化 y
  
  for (int i = 0; i < n_iter; ++i) {
    // 给定 y 采样 x，从 Binomial(n, y) 分布生成
    x = R::rbinom(n, y);
    
    // 给定 x 采样 y，从 Beta(x + a, n - x + b) 分布生成
    y = R::rbeta(x + a, n - x + b);
    
    // 存储采样结果
    samples(i, 0) = x;
    samples(i, 1) = y;
  }
  
  return samples;
}
