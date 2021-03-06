B.motion <- function(n=100, seed=FALSE, Tt=2){
    #Function that simulate a Brownian Motion
    if(seed != FALSE){set.seed(seed)}
    deltaN <- Tt/n
    B <- sqrt(deltaN)*rnorm(n, mean = 0, sd = 1)
    B <- c(0,cumsum(B))
    return(B)
}
F.mc <- function(Y, K=1, r=0, Tt=1, t=0){
    #Function that extracts, S_T, 
    #from #M simulations and finds Monte Carlo estimate of (S_T-K)^+
    m <- length(Y[[1]])
    tmp <- sapply(Y, nth, m/2)
    tmp <- tmp-K
    tmp[which( tmp <= 0)] <- 0
    OP <- exp(-r*(Tt-t))*mean(tmp, na.rm = TRUE)
    res <- list(OP = OP, process = tmp)
}
unif.ctrl <- function(x,n) runif(n, min = x[1], max = x[2])
rnd.grid <- function(n=100000,r=c(0,.04),rho=c(-.9,0),
                     alpha=c(.51,1.5),lambda =c(0,.5),sigmaV=c(.01,.8),
                     V0=c(0.05,.5),K=c(.80,1.20), Tt=c(.2,2)){
    tmp <- list(r,rho,alpha,lambda,sigmaV,V0,K,Tt)
    res <- sapply(tmp, unif.ctrl, n)
    colnames(res) <- c("r", "rho", "alpha", "lambda", "sigmaV","V0", "K", "Tt")
    return(res)
}
OPrice.MC <- function(init, n=1000, M=10000){
    Y <- vector("list",M)
    for (i in 1:M) {
        B <- cbind(B.motion(n, Tt = init[8]),B.motion(n, Tt = init[8]))
        Y[[i]] <- Y_euler_sim_cpp(B,n,0,
                                  init[8], c(1,init[6]),
                                  init[1], init[3],
                                  init[4], init[5],
                                  init[2])
    }
    names(Y) <- sprintf("Y%d",seq(1:M))
    F_mc <- F.mc(Y, K=init[7], r=init[1], Tt=init[8])
    res <- list(estimate=F_mc$OP) 
    return(res)
}
BS.OPrice <- function(S=1, K=1, Tt=1, r=0, vol=.3){
    d1 <- (log(S/K)+(r+vol^2/2)*Tt)/(vol*sqrt(Tt))
    d2 <- d1-vol*sqrt(Tt)
    C <- S*pnorm(d1)-K*exp(-r*Tt)*pnorm(d2)
    return(C)
}
IV <- function(S,K,Tt,r,vol0 = 1, C_heston){
    vol <- vol0
    fvol <- 1
    while(abs(fvol) >= 1e-10) {
        d1 <- (log(S/K)+(r+vol^2/2)*Tt)/(vol*sqrt(Tt))
        fvol <- BS.OPrice(S, K, Tt, r, vol) - C_heston
        dfvol <- S*dnorm(d1)*sqrt(Tt)
        vol = vol - fvol/dfvol;vol
    }
    return(vol)
}
vol.surf <- function(init){
    #C_heston <- OPrice.MC(init, 1000, 10000) %>% as.numeric
    C_heston <- call_heston_cf(init) %>% as.numeric
    ImpVol <- IV(1, init[7], init[8], init[1], 1, C_heston)
    return(ImpVol)
}
CalcVol <- function(init){
    ImpVol <- IV(1, init[7], init[8], init[1], 1, init[9])
    return(ImpVol)
}
take_last <- function(x){
    n <- dim(x[[1]])[1]
    res <- sapply(x, "[", n)
    return(res)
}


MC.analy <- function(init, n, M){
    Y <- vector("list",M)
    for (i in 1:M) {
        B <- cbind(B.motion(n, Tt = init[8]),B.motion(n, Tt = init[8]))
        Y[[i]] <- Y_euler_sim_cpp(B,n,0,
                                  init[8], c(1,init[6]),
                                  init[1], init[3],
                                  init[4], init[5],
                                  init[2])
    }
    names(Y) <- sprintf("Y%d",seq(1:M))
    F_mc <- F.mc(Y, K=init[7], r=init[1], Tt=init[8])
    res <- F_mc$process 
    
    mu <- mean(res)
    MC_sd <- sd(res)
    c_int <- c(mu - 1.96*MC_sd/sqrt(M), mu + 1.96*MC_sd/sqrt(M))
    
    res <- list(mu = mu, conf = c_int, vals = res, Y = Y)
}













############ HESTON CH FUNC #############
chfun_heston <- function(S0, V0, alpha, lambda, sigmaV, r, rho, Tt, w){
    i <- complex(imaginary = 1)
    
    a <- -w*w/2 - i*w/2
    b = lambda - rho*sigmaV*i*w
    gamma = sigmaV^2/2
    h = sqrt(b^2 - 4*a*gamma)
    rplus = (b + h)/sigmaV/sigmaV
    rminus = (b - h)/sigmaV/sigmaV
    g=rminus/rplus
    
    C <- lambda*( rminus*Tt-( 2/(sigmaV^2) )*log((1-g*exp(-h*Tt))/(1-g)))
    D <- rminus*(1-exp(-h*Tt))/(1-g*exp(-h*Tt))
    
    res <- exp(C*(alpha/lambda) + D*V0 + i*w*log(S0*exp(r*Tt)))
    return(res)
}

#Heston call value using characteristic functions.

call_heston_cf <- function(init, S0=1){
    r <- init[1]; rho <- init[2] 
    alpha <- init[3]; lambda <- init[4]
    sigmaV <- init[5]; V0 <- init[6]
    K <- init[7]; Tt <- init[8]; 
    
    i <- complex(imaginary = 1)
    int1.f <- function(w){
        Re( exp( -i*w*log(K) )*chfun_heston(S0, V0, alpha, lambda, sigmaV, r, rho, Tt, w-i)/
                (i*w*chfun_heston(S0, V0, alpha, lambda, sigmaV, r, rho, Tt, -i)))
    }
    int2.f <- function(w){
        Re( exp( -i*w*log(K) )*chfun_heston(S0, V0, alpha, lambda, sigmaV, r, rho, Tt, w)/
                (i*w))
    }
    #1st step: calculate pi1 and pi2
    #Inner integral 1
    
    int1 <- integrate(int1.f,0,100)
    pi1 <- int1$value/pi+0.5;
    
    int2 <- integrate(int2.f,0,100) 
    pi2 <- int2$value/pi+0.5
    
    res = S0*pi1-exp(-r*Tt)*K*pi2
    return(y)
}
#call_heston_cf(c(true_par,1,10),1)
#OPrice.MC(c(true_par,1,10), M=100000)
#NMOF::callHestoncf(1,1,10,0.02,0,0.25,.75/.25,-0.5,.25, .4)

