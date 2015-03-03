#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int timesTwo(int x) {
   return(x * 2);
}
