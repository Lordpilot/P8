setwd("C:/Users/Bruger/Desktop/P8-data") # Setting working directory

# Packages used
library(keras)
library(tensorflow)
library(plotly)
library(reshape)

# Firstly the observed Implied Volatility is defined
load("Surf_17_x_19_TRUE")
monte <- Obs_Surface[,3]

# Heatmaps using MAE, MAPE, MSE between observed and NN_QCS
NN_IV_v0.1 <- load_model_tf("NNtrue1m")
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

NNpred <- pred2
mae <- c()
mape <- c()
mse <- c()
for (i in 1:length(NNpred)) {
  mae[i] <- abs(monte[i]-NNpred[i])
  mape[i] <- abs((monte[i]-NNpred[i])/(monte[i]))*100
  mse[i] <- (monte[i]-NNpred[i])^2
}

uniK <- unique(K)
uniTt <- unique(Tt)
MAE <- matrix(mae,nrow = 19, ncol = 17)
MAPE <- matrix(mape,nrow = 19, ncol = 17)
MSE <- matrix(mse,nrow = 19, ncol = 17)
rownames(MAE) <- uniTt;rownames(MAPE) <- uniTt;rownames(MSE) <- uniTt
colnames(MAE) <- uniK; colnames(MAPE) <- uniK; colnames(MSE) <- uniK

axx = list(title = "Time to Maturity")
axy = list(title = "Strike Price")
figMAE <- plot_ly(x=~colnames(MAE), y=~rownames(MAE), z=~MAE, type = "heatmap") %>% 
  layout(title = "MAE of Implied Volatility for Strikes and Maturities",
         xaxis=axy,yaxis=axx)
figMAPE <- plot_ly(x=~colnames(MAPE), y=~rownames(MAPE), z=~MAPE, type = "heatmap") %>% 
  layout(title = "MAPE of Implied Volatility for Strikes and Maturities",
         xaxis=axy,yaxis=axx)
figMSE <- plot_ly(x=~colnames(MSE), y=~rownames(MSE), z=~MSE, type = "heatmap") %>% 
  layout(title = "MSE of Implied Volatility for Strikes and Maturities",
         xaxis=axy,yaxis=axx)
figMAE
figMAPE
figMSE

# Heatmaps using MAE, MAPE, MSE between observed and NN_MC
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

NNpred <- pred2
mae <- c()
mape <- c()
mse <- c()
for (i in 1:length(NNpred)) {
  mae[i] <- abs(monte[i]-NNpred[i])
  mape[i] <- abs((monte[i]-NNpred[i])/(monte[i]))*100
  mse[i] <- (monte[i]-NNpred[i])^2
}

uniK <- unique(K)
uniTt <- unique(Tt)
MAE <- matrix(mae,nrow = 19, ncol = 17)
MAPE <- matrix(mape,nrow = 19, ncol = 17)
MSE <- matrix(mse,nrow = 19, ncol = 17)
rownames(MAE) <- uniTt;rownames(MAPE) <- uniTt;rownames(MSE) <- uniTt
colnames(MAE) <- uniK; colnames(MAPE) <- uniK; colnames(MSE) <- uniK

axx = list(title = "Time to Maturity")
axy = list(title = "Strike Price")
figMAE <- plot_ly(x=~colnames(MAE), y=~rownames(MAE), z=~MAE, type = "heatmap") %>% 
  layout(title = "MAE of Implied Volatility for Strikes and Maturities",
         xaxis=axy,yaxis=axx)
figMAPE <- plot_ly(x=~colnames(MAPE), y=~rownames(MAPE), z=~MAPE, type = "heatmap") %>% 
  layout(title = "MAPE of Implied Volatility for Strikes and Maturities",
         xaxis=axy,yaxis=axx)
figMSE <- plot_ly(x=~colnames(MSE), y=~rownames(MSE), z=~MSE, type = "heatmap") %>% 
  layout(title = "MSE of Implied Volatility for Strikes and Maturities",
         xaxis=axy,yaxis=axx)
figMAE
figMAPE
figMSE



