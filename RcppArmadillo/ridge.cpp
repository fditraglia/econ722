// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
List ridge_cpp(mat X, colvec y, colvec lambda) {
   
   int n_lam = lambda.n_elem;
   int n = X.n_rows;
   int p = X.n_cols;
   
   mat coef(p, n_lam, fill::zeros);
   colvec y_tilde = join_cols(y, zeros<colvec>(p));
   
   for(int i = 0; i < n_lam; i++){
     mat X_tilde = join_cols(X, sqrt(lambda(i)) * eye(p, p));
     mat Q, R;
     qr_econ(Q, R, X_tilde);
     coef.col(i) = solve(R, Q.t() * y_tilde);
   }
   
  return List::create(Named("coef") = coef,
                      Named("lambda") = lambda);
}