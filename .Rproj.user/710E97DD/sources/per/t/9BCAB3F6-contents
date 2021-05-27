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