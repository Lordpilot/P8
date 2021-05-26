#include <RcppArmadillo.h>
#include <cmath>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
using namespace Rcpp;

//[[Rcpp::export()]]
arma::mat mvrnormrcpp (arma::vec mu, arma::mat Sigma, int n) {
    int d = mu.n_elem;
    int Sr = Sigma.n_rows;
    if(d != Sr){
        stop("Wrong dimensions");
    }
    else{
        arma::mat std_norm(d,n,arma::fill::randn);
        arma::mat cholS = arma::chol(Sigma);
        arma::mat res = arma::trans(cholS)*std_norm;
        res.each_col() += mu;
        return(res);
    }
}


//[[Rcpp::export]]
arma::mat Y_euler_sim_cpp(arma::mat B, int n, 
                          int t, double Tt, arma::vec y0,
                          double r, double alpha, double lambda, double sigmaV, double rho) {
    arma::vec tmesh = linspace(t,Tt,n+1);
    arma::vec mesh_dist = diff(tmesh);
    
    int mm = B.n_rows - 1;
    arma::vec tmp = linspace(0,mm,n+1);
    arma::uvec index = conv_to< uvec >::from(tmp);
    arma::mat B_tmp = B.rows(index);
    arma::mat B_inc = diff(B_tmp,1,0);
    
    arma::mat Y(n+1,y0.n_elem); Y.zeros(); 
    Y(0,0) = y0(0); Y(0,1) = y0(1);
    
    
    for(int i = 1; i < n+1; i++){
        Y(i,0) = Y(i-1,0) + 
            Y(i-1,0)*r*mesh_dist[i-1] + 
            Y(i-1,0)*sqrt(Y(i-1,1))*(sqrt(1-pow(rho,2))*B_inc(i-1,0) + rho*B_inc(i-1,1));
            Y(i,1) = Y(i-1,1) + (alpha - lambda*Y(i-1,1))*mesh_dist[i-1] + sigmaV*sqrt(Y(i-1,1))*B_inc(i-1,1);
    }
    
    
    return Y;
}
