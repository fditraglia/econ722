// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
vec softmax_cpp(vec x, vec y) {
  return sign(x) % max(abs(x) - y, zeros(x.n_elem));
}

// [[Rcpp::export]]
List lasso_shoot_cpp(mat X, vec y, double lambda,
                    double tol = 1e-5, int max_iter = 10000){
  int p = X.n_cols;
  mat XX = X.t() * X;
  vec Xy = X.t() * y;
  vec Xy2 = 2 * Xy;
  mat XX2 = 2 * XX;
  
  vec beta = solve(XX + diagmat(lambda * ones(p)), Xy);
  
  bool converged = false;
  int iteration = 0;
  vec beta_prev, aj, cj;
  
  while (!converged && (iteration < max_iter)){
    
    beta_prev = beta;
    
    for (int j = 0; j < p; j++){
      aj = XX2(j,j);
      cj = Xy2(j) - dot(XX2.row(j), beta) + beta(j) * XX2(j,j);
      beta(j) = as_scalar(softmax_cpp(cj / aj, lambda / aj));
    }
    iteration = iteration + 1;
    converged =  norm(beta_prev - beta, 1) < tol;  
  }
  return List::create(Named("beta") = beta,
                      Named("n_iter") = iteration,
                      Named("converged") = converged);
}