rm(list = ls())
library(lubridate)
library(timetk)
library(purrr)
library(quantmod)
library(lpSolve)
#
devtools::install_github('joshuaulrich/xts', force = T)
#
devtools::install_github('systematicinvestor/SIT.date', force = T)
curl::curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'SIT.tar.gz',mode = 'wb',quiet=T)
install.packages('SIT.tar.gz', repos = NULL, type='source')
library(SIT)
library(PerformanceAnalytics)
library(xts)
library(tidyr)
Import data

tickers = spl('GLD,MSI,EPI,XLE,EWV,DIA,MCD,XRT')

datas <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = datas, auto.assign = T)
for(i in ls(datas)) datas[[i]] = adjustOHLC(datas[[i]], use.Adjusted=T)
names(datas)
datas$MSI
head(datas$MSI)
#
bt.prep(datas, align='remove.na')
names(datas)
#
head(datas$GLD)
head(datas$prices)
#
prices_monthly <- to.monthly(datas$prices, indexAt = "last", OHLC = FALSE) # indexAt = 'endof'
head(prices_monthly)
monthly.ret <- na.omit(Return.calculate(prices_monthly, method = "discrete"))
head(monthly.ret)
Download Fama-French factors from website

url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
temp <- tempfile()
download.file(url, temp, method = "libcurl", mode = "wb")
unzip(temp, "F-F_Research_Data_Factors_daily.CSV")
unlink(temp)
#
mydata <- read.csv("F-F_Research_Data_Factors_daily.CSV", skip = 4)
mydata <- mydata[-nrow(mydata), ]  # remove last row
fama_lib <- xts(x = mydata[, c(2,3,4)], order.by = as.Date(paste(mydata[, 1]), "%Y%m%d"))
str(fama_lib)
head(fama_lib)
#
dates <- '2005-01::2021-03'
X <- monthly.ret[dates]
head(X)
dim(X)
#
fama_lib_month <- to.monthly(fama_lib, indexAt = "last", OHLC = FALSE) # indexAt = 'endof'
head(fama_lib_month)
f <- fama_lib_month[dates]/100
head(f)
dim(f)
Use Professor Palomar’s package to compute estimated coefficients Based on CAPM model, compute covariance matrix for the 8-asset portfolio by using past 60-month returns from 2010/01 - 2014/12.

devtools::install_github("dppalomar/covFactorModel")
library(covFactorModel)
# 
insample_range <- '2010-01::2014-12'
one_factor_model <- factorModel(X[insample_range], type = "M", econ_fact = f$Mkt.RF[insample_range])
names(one_factor_model)
cbind(alpha = one_factor_model$alpha, beta = one_factor_model$beta)
#======================================================================
# we can do the fitting using a more compact matrix notation
F_ <- cbind(ones = 1, f$Mkt.RF[insample_range])
Gamma <- t(X[insample_range]) %*% F_ %*% solve(t(F_) %*% F_)  # better: Gamma <- t(solve(t(F_) %*% F_, t(F_) %*% X))
colnames(Gamma) <- c("alpha", "beta")
alpha <- Gamma[, 1]  # or alpha <- Gamma[, "alpha"]
beta <- Gamma[, 2]   # or beta <- Gamma[, "beta"]
print(Gamma)
# compute Sigma
X <- X[insample_range]
f <- f$Mkt.RF[insample_range]
T <- dim(X)[1]
E <- xts(t(t(X) - Gamma %*% t(F_)), index(X))  # residuals
Psi <- (1/(T-2)) * t(E) %*% E
Sigma <- as.numeric(var(f)) * beta %o% beta + diag(diag(Psi))
Sigma # This is covariance matrix computed from single factor model
#=======================================================================
# We can also use lm() to compute estimated coefficients
fit = lm(formula = X~f)
sigF = as.numeric(var(f))
beta_ = as.matrix(fit$coefficients)
beta_ = as.matrix(beta_[-1,])
beta_
sigeps = crossprod(fit$residuals)/(T-2)
# sigeps = as.matrix(var(fit$residuals)) #  you can use this way too
sigeps = diag(diag(sigeps))
sigeps
cov_1f = sigF*beta_%*%t(beta_)+sigeps
cov_1f # This is covariance matrix computed from single factor model (from lm())
Backtesting portfolio using SIT package

dates <- '2005-01::2021-03'
X <- monthly.ret[dates]
head(X)
dim(X)
tail(X)
#
prices_monthly <- to.monthly(datas$prices, indexAt = "last", OHLC = FALSE) # indexAt = 'endof'
head(prices_monthly)
prices_monthly <- prices_monthly[dates]
head(prices_monthly)
tail(prices_monthly)
#
#*****************************************************************
# Code Strategies
#****************************************************************** 
# We need to create environment variable required by SIT 
data_m <- new.env()
#
# data_m$EEM <- prices_monthly$EEM
# colnames(data_m$MSI) <- 'Close'
# head(data_m$MSI)
# Change column names to Close which will be consistent with the requirement of SIT package
data_m$GLD <- prices_monthly$GLD %>% `colnames<-` (c("Close"))
data_m$MSI <- prices_monthly$MSI %>% `colnames<-` (c("Close"))
data_m$EPI <- prices_monthly$EPI %>% `colnames<-` (c("Close"))
data_m$XLE <- prices_monthly$XLE %>% `colnames<-` (c("Close"))
data_m$EWV <- prices_monthly$EWV %>% `colnames<-` (c("Close"))
data_m$DIA <- prices_monthly$DIA %>% `colnames<-` (c("Close"))
data_m$MCD <- prices_monthly$MCD %>% `colnames<-` (c("Close"))
data_m$XRT <- prices_monthly$XRT %>% `colnames<-` (c("Close"))
names(data_m)
head(data_m$MSI)
# Using loop to save time in naming 
data_m <- list()
#i = 1
for ( i in 1:length(tickers)){
  data_m[[tickers[i]]] <- prices_monthly[,i] %>% `colnames<-` (c("Close"))
}

# data_env <- new.env()
# list2env(data_m, envir = data_env)
# convert from list to environment variable because SIT package requires 
# the input data to be environmental variable which can be processed using bt.prep()
data_m <- list2env(data_m)
names(data_m)
# Check if the column name is 'Close'
head(data_m$MSI)
#
bt.prep(data_m, align='remove.na', dates = dates)
names(data_m)
#===============================================================
# Equal weighting portfolio
# Equal Weight 1/N Benchmark
#===============================================================
models <- list()
prices <- data_m$prices
# data_m$weight[] = NA
N <- length(tickers)
data_m$weight = ntop(prices, N)       
head(data_m$weight)
data_m$weight[1:59, ] <- NA
models$equal.weight = bt.run.share(data_m)
# head(models$equal.weight$ret, 62)
# Slightly difference between bt.run.share() and bt.run() 
capital = 100000
data_m$weight[] = (capital / prices) * data_m$weight
models$equal.weight.share = bt.run(data_m, type='share')
# head(models$equal.weight.share$ret, 62)
# head(equal.weight$ret)
#================================================================
# MVP portfolio
#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(N, lb = -Inf, ub = +Inf)
# SUM x.i = 1
constraints = add.constraints(rep(1, N), 1, type = '=', constraints)        

#
ret = prices / mlag(prices) - 1
weight = coredata(ret)
weight[] = NA
# 
# head(data_m$weight)
# compute covariance matrix based on historical 60 months returns
# i = 60
# To make covariance matrix estimate more stable, use the Ledoit-Wolf covariance shrinkage estimator from tawny package
# 1. ia$cov = tawny::cov.shrink(hist)  or
# 2. ia$cov = cor(coredata(hist), use='complete.obs', method='spearman') * (s0 %*% t(s0)) or
# 3. ia$cov = cor(coredata(hist), use='complete.obs', method='kendall') * (s0 %*% t(s0)
# i = 60
for( i in 60:dim(weight)[1]) {
  hist = ret[ (i- 60 + 1):i, ]
  # create historical input assumptions
  ia = create.historical.ia(hist, 12) # 12 is annulized factor for monthly returns
  s0 = apply(na.omit(coredata(hist)), 2, sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
  weight[i,] = min.risk.portfolio(ia, constraints) # use SIT's function min.risk.portfolio()
}
dim(weight)
# 195 8
head(weight, 70)
tail(weight)
# Assign minimum variance weights to data_m$weight
capital = 100000
data_m$weight <- data_m$prices
data_m$weight[] <- NA
data_m$weight[] <- weight     
data_m$weight[] = (capital / prices) * data_m$weight
models$mvp.hist.cov = bt.run(data_m, type='share')
CAPM model (single factor model)

# Create Constraints
#*****************************************************************
constraints = new.constraints(N, lb = -Inf, ub = +Inf)
# SUM x.i = 1
constraints = add.constraints(rep(1, N), 1, type = '=', constraints)        

#
ret = prices / mlag(prices) - 1
weight_1 = coredata(ret)
weight_1[] = NA
# 
# head(data_m$weight)
# compute covariance matrix based on historical 60 months returns
# i = 60
f <- fama_lib_month[dates]/100

for( i in 60:dim(weight_1)[1]) {
  hist = ret[ (i- 60 + 1):i, ]
  Xi <- hist
  fi <- f$Mkt.RF[(i - 60 + 1):i, ]
  fiti = lm(formula = Xi ~ fi)
  sigF = as.numeric(var(fi))
  beta_ = as.matrix(fiti$coefficients)
  beta_ = as.matrix(beta_[-1,])
  sigeps = crossprod(fiti$residuals)/(T-2)
  # sigeps = as.matrix(var(fit$residuals)) #  you can use this way too
  sigeps = diag(diag(sigeps))
  cov_1f = sigF*beta_%*%t(beta_)+sigeps
  # cov_1f
  ia$cov = cov_1f
  weight_1[i, ] = min.risk.portfolio(ia, constraints)
}

dim(weight_1)
# 195 8
head(weight_1, 70)
tail(weight_1)
# Assign minimum variance weights to data_m$weight
capital = 100000
data_m$weight <- data_m$prices
data_m$weight[] <- NA
data_m$weight[] <- weight_1     
data_m$weight[] = (capital / prices) * data_m$weight
models$mvp.capm.cov = bt.run(data_m, type='share')
models$mvp.capm.cov$cagr
models$mvp.hist.cov$cagr
models$mvp.hist.cov$cagr
FF 3 factor model

# Create Constraints
#*****************************************************************
constraints = new.constraints(N, lb = -Inf, ub = +Inf)
# SUM x.i = 1
constraints = add.constraints(rep(1, N), 1, type = '=', constraints)        

#
ret = prices / mlag(prices) - 1
weight_3 = coredata(ret)
weight_3[] = NA
# 
# head(data_m$weight)
# compute covariance matrix based on historical 60 months returns
# i = 60
f <- fama_lib_month[dates]/100

for( i in 60:dim(weight_1)[1]) {
  hist = ret[ (i- 60 + 1):i, ]
  Xi <- hist
  fi <- f[(i - 60 + 1):i, ]
  fiti = lm(formula = Xi ~ fi)
  sigF = as.matrix(var(fi))
  beta_ = as.matrix(fiti$coefficients)
  beta_ = as.matrix(beta_[-1,])
  sigeps = crossprod(fiti$residuals)/(T-4) # note (T - 4)
  # sigeps = as.matrix(var(fit$residuals)) #  you can use this way too
  sigeps = diag(diag(sigeps))
  cov_3f = t(beta_)%*% sigF %*% beta_ + sigeps
  # cov_1f
  ia$cov = cov_3f
  weight_3[i, ] = min.risk.portfolio(ia, constraints)
}

dim(weight_3)
# 195 8
head(weight_3, 70)
tail(weight_3)
# Assign minimum variance weights to data_m$weight
capital = 100000
data_m$weight <- data_m$prices
data_m$weight[] <- NA
data_m$weight[] <- weight_3     
data_m$weight[] = (capital / prices) * data_m$weight
models$mvp.ff3.cov = bt.run(data_m, type='share')
Principal Component Analysis (PCA)

library(covFactorModel)
X_i <-X[1:60,] 
factor_pca <- factorModel(X_i, type = "S", K = 3, max_iter = 10)
cbind(alpha = factor_pca$alpha, beta = factor_pca$beta)
#
# Statistical 3-factor model
K <- 3
X_trn <- X_i
T_trn <- dim(X_i)[1]
alpha <- colMeans(X_trn)
X_trn_ <- X_trn - matrix(alpha, T_trn, N, byrow = TRUE)
Sigma_prev <- matrix(0, N, N)
Sigma <- (1/(T_trn-1)) * t(X_trn_) %*% X_trn_
eigSigma <- eigen(Sigma)
while (norm(Sigma - Sigma_prev, "F")/norm(Sigma, "F") > 1e-3) {
  B <- eigSigma$vectors[, 1:K] %*% diag(sqrt(eigSigma$values[1:K]), K, K)
  Psi <- diag(diag(Sigma - B %*% t(B)))
  Sigma_prev <- Sigma
  Sigma <- B %*% t(B) + Psi
  eigSigma <- eigen(Sigma - Psi)
}
# B: factor loadings
B
# fiti = lm(formula = X_i ~ t(B))
Sigma_PCA3 <- Sigma
Sigma_PCA3
diag(Sigma_PCA3)
# error
norm(Sigma_PCA3 - cov(X_i), "F")
#---------------------------------------------------------------
# By Eric Zivot
# use R princomp() function for principal component analysis
#---------------------------------------------------------------
pc.fit = princomp(X_i)
class(pc.fit)
pc.fit
summary(pc.fit)
eig.vec <- eigen(cov(X_i))$vectors
#
B_hat <- eig.vec[, 1:3]%*%diag(sqrt(eig.value[1:3]), 3, 3)
B_hat
#
#
plot(pc.fit)
loadings(pc.fit)
pc.fit$loadings[, 1:3]
# pc factors are in the scores component. Note these scores are based on
# centered data
head(pc.fit$scores[, 1:3])
pc.fit$scores[, 1:3]
# time series plot of principal component factors
# library(PerformanceAnalytics)
# chart.TimeSeries(pc.fit$scores[, 1, drop=FALSE], colorset="blue")
# compare with direct eigen-value analysis
# notice the sign change on the first set of loadings
eigen.fit = eigen(var(X_i))
names(eigen.fit)
names(eigen.fit$values) = rownames(eigen.fit$vectors) = colnames(X_i)
cbind(pc.fit$loadings[,1:3], eigen.fit$vectors[, 1:3])
# compute uncentered pc factors from eigenvectors and return data
pc.factors.uc = X_i %*% eigen.fit$vectors
colnames(pc.factors.uc) = paste(colnames(pc.fit$scores),".uc",sep="")
# compare centered and uncentered scores. Note sign change on first factor
# We can treat centered scores as unobservable factor values (F)
cbind(pc.fit$scores[,1,drop=F], -pc.factors.uc[, 1, drop=F])
#
# use first 3 eigen-vectors to compue three factor (with normalization to have pos correlation with market)
# note: cannot treat pc as a portfolio b/c weights do not sum to unity
p3 = pc.fit$loadings[, 1:3]
p3 # refers to B
colSums(p3)
# create factor mimicking portfolio by normalizing weights to unity
p3 = p3/colSums(p3)
p3
barplot(p3[,1], horiz=F, main="Factor mimicking weights", col="blue", cex.names = 0.75, las=2)
# create first 3 factors
f3 = X_i %*% p3
head(f3)
head(pc.fit$scores[, 1:3])
# chart.TimeSeries(f1, main="First principal component factor", colorset="blue")
# estimate factor betas by multivariate regression
n.obs <- dim(X_i)[1]
X.mat = cbind(rep(1, n.obs), f3)
colnames(X.mat) = c("intercept", "Factor 1", "Factor 2", "Factor 3")
XX.mat = crossprod(X.mat)
# multivariate least squares
G.hat = solve(XX.mat)%*%crossprod(X.mat, X_i)
t(G.hat)
# can also use solve(qr(X.mat), returns.mat)
beta.hat = G.hat[2:4,]
beta.hat
B
E.hat = X_i - X.mat%*%G.hat
diagD.hat = diag(crossprod(E.hat)/(n.obs-4))
# compute covariance/correlation matrices with three pc factor
cov.pc3 = t(beta.hat)%*%var(f3)%*%(beta.hat) + diag(diagD.hat)
cov.pc3
Sigma_PCA3
diag(cov.pc3)
# error difference between pca and empirical covariance matrix
norm(cov.pc3 - cov(X_i), "F")
norm(Sigma_PCA3 - cov(X_i), "F")
Plot performance

plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)
Plot Strategy Statistics Side by Side

layout(1:1)
plotbt.strategy.sidebyside(models)
Plot transition maps

layout(1:len(models))
for(m in names(models)) {
  plotbt.transition.map(models[[m]]$weight, name=m)
  legend('topright', legend = m, bty = 'n')
}


