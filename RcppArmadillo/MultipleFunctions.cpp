// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;



// [[Rcpp::export]]
mat cppVAR1(mat B, int n) {
  int p = B.n_cols;
  mat y(p, n, fill::zeros);
   for (int i = 1; i < n; i++) { //zero-indexing!
     colvec errors(rnorm(p));
     y.col(i) = B * y.col(i - 1) + errors;
   }
   return y;
}