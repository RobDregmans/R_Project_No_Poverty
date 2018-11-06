library(rjags)


broadband <- read.table("data/broadband_access_log.csv",header=TRUE,sep=",")
bbnd <- broadband$IT.NET.BBND
gdp <- broadband$NY.GDP.MKTP.CD
pop <- broadband$SP.POP.TOTL
rail <- broadband$IS.RRS.TOTL.KM

n <- nrow(broadband)

# standardize variables
bbnd <- (bbnd-mean(bbnd))/sd(bbnd)
pop <- (pop-mean(pop))/sd(pop)
gdp <- (gdp-mean(gdp))/sd(gdp)
rail <- (rail-mean(rail))/sd(rail)

# convert chains to matrix
samp=readRDS("example.rds")
sampmatrix = as.matrix(samp)

# take the first four parameters, but drop off the noise!
param <-sampmatrix[sample(nrow(sampmatrix),size = 100, replace=FALSE),1:4]

# create a design matrix
n <- nrow(broadband)
# make an intercept
intercept <- rep(1,n)
# bind your data together in the order used for modelling
data <- cbind(intercept,gdp,rail,pop)
aaaa <- t(data)
tada <- param %*% t(data)
pred <- t(tada)
# replicate the results
act <- rep(bbnd,100)
par(mar=c(1,1,1,1))
dev.off()
plot(act,pred,xlab="Actual",ylab="Predicted")
