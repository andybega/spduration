#include <RcppArmadillo.h>
using namespace arma; 
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

//' Split-population Weibull log likelihood
//' 
//' @param theta Current parameter values. 
//' @param y Dependent variables. 
//' @param X Duration equation covariates.
//' @param Z Risk equation covariates.
//' 
//' @keywords internal
// [[Rcpp::export]]
double spweib_lnl(arma::vec theta, arma::mat y, arma::mat X, arma::mat Z) {
  
  int rx = X.n_cols - 1;
  int rz = Z.n_cols - 1;
  vec beta = theta.subvec(0, rx);
  vec gamma = theta.subvec(rx + 1, rx + rz + 1);
  double p = theta[rx + rz + 2];
  vec lambda = exp(-X * beta);
  double alpha = exp(-p);

  vec rc = y.submat(0,0,y.n_rows-1,0);
  vec ti = y.submat(0,1,y.n_rows-1,1); 
  vec lo = y.submat(0,2,y.n_rows-1,2); 
  vec t0 = y.submat(0,3,y.n_rows-1,3); 

  vec pr0 = 1/(1 + exp( Z * gamma ));
  vec pr1 = 1 - pr0;

  vec lnFt = log(alpha) + alpha*log(lambda) + (alpha-1)*log(ti) - pow(lambda % ti, alpha);
  vec st = exp( - pow(lambda % ti, alpha) );
  vec st0 = exp( - pow(lambda % t0, alpha));

  vec cens(lambda.size());
  for(int i=0; i<cens.size(); ++i) {
    if ( lo[i]==0 | rc[i]==0 ) {
      cens[i] = log(pr0[i] + (pr1[i] * st[i])) - log(pr0[i] + pr1[i] * st0[i]);
    } else {
      cens[i] = 0;
    }
  }

  vec nocens(lambda.size());
  for(int i=0; i<nocens.size(); ++i) {
    if ( rc[i]==1 & lo[i]==1 ) {
      nocens[i] = log(pr1[i]) + lnFt[i] - log(pr0[i] + pr1[i] * st0[i]);
    } else {
      nocens[i] = 0;
    }
  }

  double logL = sum(cens + nocens);
  return(-logL);
}
