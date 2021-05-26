#include <RcppArmadillo.h>
#include <cmath>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
using namespace Rcpp;
using namespace R;



//[[Rcpp::export()]]
double BS_OPrice_cpp (double S, double K, double Tt, double r, double vol){
    using Rcpp::pnorm;
    double d1 = (log(S/K)+(r-pow(vol,2)/2)*Tt)/(vol*sqrt(Tt));
    double d2 = d1-vol*sqrt(Tt);
    double C_BS = S*R::pnorm(d1, 0.0, 1.0, true, false)-K*exp(-r*Tt)*R::pnorm(d2, 0.0, 1.0, true, false);
    return(C_BS);
}

void BS_OPrice_cpp();
//[[Rcpp::export()]]
double IV_cpp(double S, double K, double Tt, double r, double vol0, double C_heston){
    double vol = vol0;
    double fvol = 1;
    while((abs(fvol) >= 0.0001)) {
        double d1 = (log(S/K)+(r+pow(vol,2)/2)*Tt)/(vol*sqrt(Tt));
        double fvol = BS_OPrice_cpp(S, K, Tt, r, vol) - C_heston;
        double dfvol = S*R::dnorm(d1, 0.0, 1.0, false)*sqrt(Tt);
        double vol = vol - fvol/dfvol;
    }
    return(vol);
}


