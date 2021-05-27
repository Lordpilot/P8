setwd("C:/Users/Bruger/Desktop/P8-data") # Setting working directory

# Packages used
library(keras)
library(tensorflow)
library(plotly)
library(reshape)

# The Implied Volatility surface for the NN_QCS
load("bodil") # Load Option prices
NN_IV_v0.1 <- load_model_tf("NNtrue1m")
test_par <- BO_DATA[[1]]
test_target <- BO_DATA[[2]]
init <- test_par
tar <- test_target
plot(tar[1:100])
pred <- NN_IV_v0.1 %>% 
  predict_on_batch(init)
points(pred, col = "red")
n <- 19*19
r <- rep(0.02,n)
rho <- rep(-0.5, n)
alpha <- rep(0.75, n)
lambda <- rep(0.25, n)
sigmaV <- rep(0.4, n)
V0 <- rep(0.25,n)
K <- rep(seq(0.8,1.2,0.4/18), each=19)
Tt <- rep(seq(0.2,2,0.1),19)
Dat <- cbind(r, rho, alpha, lambda, sigmaV, V0, K, Tt)
pred2 <- predict_on_batch(NN_IV_v0.1, Dat)

IV1 <- cbind(K,Tt,pred2)
uniK <- unique(K)
uniTt <- unique(Tt)
IV <- matrix(pred2,nrow = 19, ncol = 19)
rownames(IV) <- uniTt
colnames(IV) <- uniK

# Plot of Implied Volatility Surface for NN_QCS
axx = list(title = "x: Time to Maturity")
axy = list(title = "y: Strike Price")
axz = list(title = "z: Implied Volatility")
plot_ly(x=~colnames(IV), y=~rownames(IV), z=~IV) %>% 
  add_surface() %>%
  layout(title = "Implied Volatility for Strike and Maturity",
         scene = list(xaxis=axy,yaxis=axx,zaxis=axz))

# Implied Volatility Surface for NN_MC 
NN_IV_v0.1 <- load_model_tf("NNmc1m")
n <- 17*19
r <- rep(0.02,n)
rho <- rep(-0.5, n)
alpha <- rep(0.75, n)
lambda <- rep(0.25, n)
sigmaV <- rep(0.4, n)
V0 <- rep(0.25,n)
K <- Obs_Surface[,1]
Tt <- Obs_Surface[,2]
Dat <- cbind(r, rho, alpha, lambda, sigmaV, V0, K, Tt)
pred2 <- predict_on_batch(NN_IV_v0.1, Dat)

uniK <- unique(K)
uniTt <- unique(Tt)
IV <- matrix(pred2,nrow = 19, ncol = 17)
rownames(IV) <- uniTt
colnames(IV) <- uniK

# Plot for Implied Volatility Surface of NN_MC
axx = list(title = "x: Time to Maturity")
axy = list(title = "y: Strike Price")
axz = list(title = "z: Implied Volatility")
plot_ly(x=~colnames(IV), y=~rownames(IV), z=~IV) %>% 
  add_surface() %>%
  layout(title = "Implied Volatility for Strike and Maturity",
         scene = list(xaxis=axy,yaxis=axx,zaxis=axz))

# Observed Implied Volatility Surface
load("Surf_17_x_19_TRUE")
K <- Obs_Surface[,1]
Tt <- Obs_Surface[,2]

IV <- Obs_Surface[,3]
IV <- matrix(IV,nrow = 19, ncol = 17)
uniK <- unique(K)
uniTt <- unique(Tt)
rownames(IV) <- uniTt
colnames(IV) <- uniK

# Plot of the observed Implied Volatility Surface
axx = list(title = "x: Time to Maturity")
axy = list(title = "y: Strike Price")
axz = list(title = "z: Implied Volatility")
plot_ly(x=~colnames(IV), y=~rownames(IV), z=~IV) %>% 
  add_surface() %>%
  layout(title = "Implied Volatility for Strike and Maturity",
         scene = list(xaxis=axy,yaxis=axx,zaxis=axz))



