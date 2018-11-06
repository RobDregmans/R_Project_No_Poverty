library(rjags)

broadband <- read.table("broadband_access_log.csv",header=TRUE,sep=",")
bbnd <- broadband$IT.NET.BBND
gdp <- broadband$NY.GDP.MKTP.CD
pop <- broadband$SP.POP.TOTL
urb <- broadband$AG.LND.TOTL.UR.K2/broadband$AG.LND.TOTL.K2
n <- nrow(broadband)

# log-odds trick
# converts proportion to ratio scale
urb <- log(urb/(1-urb)) 

# standardize variables
bbnd <- (bbnd-mean(bbnd))/sd(bbnd)
pop <- (pop-mean(pop))/sd(pop)
gdp <- (gdp-mean(gdp))/sd(gdp)

# We run with a simple three node model for demonstration
model_string <- "model{

# Likelihood
for(i in 1:n){
bbnd[i]   ~ dnorm(mu[i],inv.var1)
mu[i] <- beta1 + beta2*gdp[i] + beta3*urb[i]
}

# Prior for beta
beta1 ~ dnorm(0,inv.var2)
beta2 ~ dnorm(0,inv.var3)
beta3 ~ dnorm(0,inv.var4)


# Prior for the inverse variance
var1   ~ dunif(0.01, 0.99)
var2   ~ dunif(0.01, 0.99)
var3   ~ dunif(0.01, 0.99)
var4   ~ dunif(0.01, 0.99)
inv.var1   <- 1/var1
inv.var2   <- 1/var2
inv.var3   <- 1/var3
inv.var4   <- 1/var4
sigma1     <- sqrt(var1)
sigma2     <- sqrt(var2)
sigma3     <- sqrt(var3)
sigma4     <- sqrt(var4)
}"


model <- jags.model(textConnection(model_string), data = list(urb=urb,gdp=gdp,bbnd=bbnd,n=n))

update(model, 10000, progress.bar="none"); # Burn-in for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta1","beta2","beta3","beta4","sigma1","sigma2","sigma3","sigma4" ), 
                     n.iter=20000, progress.bar="text")

summary(samp)
saveRDS(samp,"example.rds")