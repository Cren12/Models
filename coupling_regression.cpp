// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat coupling_regression(const arma::vec& target,
                              const arma::mat& regressors,
                              double min_sample,
                              double max_sample)
{
  arma::mat coupling_coefficients;
  for (double i = min_sample; i < max_sample + 1; i++)
  {
    arma::mat X = regressors.head_rows(i);
    arma::vec y = target.head(i);
    arma::colvec first_coef = arma::solve(X, y);
    X = regressors.tail_rows(i);
    y = target.tail(i);
    arma::colvec second_coef = arma::solve(X, y);
    double first_beta = first_coef[0];
    double second_beta = second_coef[0];
    coupling_coefficients = arma::join_vert(coupling_coefficients, 
                                            arma::rowvec({i, fabs(second_beta - first_beta)}));
  }
  return coupling_coefficients;
}
