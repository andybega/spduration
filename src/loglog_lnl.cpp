#include <RcppArmadillo.h>
using namespace arma; 
using namespace Rcpp; 

// [[Rcpp::depends(RcppArmadillo)]]

//' Regular Log-logistic likelihood
//' 
//' @param theta Current parameter values. 
//' @param y Dependent variables. 
//' @param X Duration equation covariates.
//' 
//' @keywords internal
// [[Rcpp::export]]
double loglog_lnl(arma::vec theta, arma::mat y, arma::mat X) {
  
  int ncols = X.n_cols - 1;
  vec beta = theta.subvec(0, ncols);
  vec lambda = exp(-X * beta);
  double p = theta[ncols+1];
  double alpha = exp(-p);
  
  vec rc = y.submat(0,0,y.n_rows-1,0);
  vec ti = y.submat(0,1,y.n_rows-1,1); 
  vec lo = y.submat(0,2,y.n_rows-1,2); 
  vec t0 = y.submat(0,3,y.n_rows-1,3); 
  
  vec lnFt = log(alpha) + alpha*log(lambda) + (alpha-1)*log(ti) - 2 * log(1 + pow(lambda % ti, alpha));
  vec lnSt = -log(1 + pow(lambda % ti, alpha));
  vec lnSt0 = -log(1 + pow(lambda % t0, alpha));

  vec cens(lambda.size());
  for(int i=0; i<cens.size(); ++i) {
    if ( rc[i]==0 || lo[i]==0 ) {
  		cens[i] = lnSt[i] - lnSt0[i];
  	} else {
  		cens[i] = 0;
  	}
  }

  vec nocens(lambda.size());
  for(int i=0; i<nocens.size(); ++i) {
    if ( rc[i]==1 && lo[i]==1 ) {
  		nocens[i] = lnFt[i] - lnSt0[i];
  	} else {
  		nocens[i] = 0;
  	}
  }

  double logL = sum(cens + nocens);
  return(-logL);
}

