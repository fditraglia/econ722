// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

mat f(mat A){
  mat out = A.t() * A;
  return(out);
}

mat g(mat B){
  mat out = f(B) + f(B);
  return(out);
}

// [[Rcpp::export]]
double h(mat C){
  double out = trace(g(C));
  return(out);
}