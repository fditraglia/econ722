// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
mat mvrnorm(int n, vec mu, mat Sigma){
/*-------------------------------------------------------
# Generate draws from a multivariate normal distribution
#--------------------------------------------------------
#  n        number of samples
#  mu       mean vector
#  Sigma    covariance matrix
#-------------------------------------------------------*/
  RNGScope scope;
  int p = Sigma.n_cols;
  mat X = reshape(vec(rnorm(p * n)), p, n);
  vec eigval;
  mat eigvec;
  eig_sym(eigval, eigvec, Sigma);
  X = eigvec * diagmat(sqrt(eigval)) * X;
  X.each_col() += mu;
  return(X.t());
}

/*-------------------------------------------------------
# Generate Draws from an Inverse Wishart Distribution
# via the Bartlett Decomposition
#--------------------------------------------------------
# NOTE: output is identical to riwish from MCMCpack
#       provided the same random seed is used
#--------------------------------------------------------
#   n     number of samples
#   S     scale matrix 
#   v     degrees of freedom
#-------------------------------------------------------*/
// [[Rcpp::export]]
cube rinvwish(int n, int v, mat S){
  RNGScope scope;
  int p = S.n_rows;
  mat L = chol(inv_sympd(S), "lower");
  cube sims(p, p, n, fill::zeros);
  for(int j = 0; j < n; j++){
  mat A(p,p, fill::zeros);
  for(int i = 0; i < p; i++){
    int df = v - (i + 1) + 1; //zero-indexing
    A(i,i) = sqrt(R::rchisq(df)); 
  }
  for(int row = 1; row < p; row++){
    for(int col = 0; col < row; col++){
      A(row, col) = R::rnorm(0,1);
    }
  }
  mat LA_inv = inv(trimatl(trimatl(L) * trimatl(A)));
  sims.slice(j) = LA_inv.t() * LA_inv;
  }
  return(sims);
}