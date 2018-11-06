#selecting data from wdi
colnames(our_indicator_data) = c("Country.Code","Region","Country.Name","Indicator.Code","Indicator.Name","Year", "value")
var_jags = c("SI.POV.DDAY","SP.DYN.LE00.IN","SN.ITK.DEFC.ZS","SH.DYN.MORT")

#subset the data with only the world and the two indicators above about population and people in poverty in %
jags_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_jags)
jags_data = subset(jags_data, select = c("Indicator.Code","Country.Name", "Country.Code", "Year", "value"))
jags_data = dcast(jags_data,Country.Code + Country.Name + Year ~ Indicator.Code, value.var ="value")
df_jags_year = subset(jags_data, jags_data$Year==2012)
df_jags_year = df_jags_year[-which(rowSums(is.na(df_jags_year))> 0),]

poverty = df_jags_year$SI.POV.DDAY
life_expacteny = df_jags_year$SP.DYN.LE00.IN
undernourishment = df_jags_year$SN.ITK.DEFC.ZS
child_mortality = df_jags_year$SH.DYN.MORT
n = nrow(df_jags_year)

x <- poverty
y1 <- life_expacteny
y2 <- undernourishment
y3 <- child_mortality

library(rjags)

x <- (x-mean(x))/sd(x)
y1 <- (y1-mean(y1))/sd(y1)
y2 <- (y2-mean(y2))/sd(y2)
y3 <- (y3-mean(y3))/sd(y3)

model_string <- "model{

# Likelihood
for(i in 1:n){
x[i]   ~ dnorm(mu[i],inv.var1)
mu[i] <- beta1 + beta2*y1[i] + beta3*y2[i] + beta4*y3[i]
}

# Prior for beta
beta1 ~ dnorm(0,inv.var2)
beta2 ~ dnorm(0,inv.var3)
beta3 ~ dnorm(0,inv.var4)
beta4 ~ dnorm(0,inv.var5)


# Prior for the inverse variance
var1   ~ dunif(0.01, 0.99)
var2   ~ dunif(0.01, 0.99)
var3   ~ dunif(0.01, 0.99)
var4   ~ dunif(0.01, 0.99)
var5   ~ dunif(0.01, 0.99)
inv.var1   <- 1/var1
inv.var2   <- 1/var2
inv.var3   <- 1/var3
inv.var4   <- 1/var4
inv.var5   <- 1/var5
sigma1     <- sqrt(var1)
sigma2     <- sqrt(var2)
sigma3     <- sqrt(var3)
sigma4     <- sqrt(var4)
sigma5     <- sqrt(var5)
}"

model <- jags.model(textConnection(model_string), data = list(x=x, y1=y1,y2=y2,y3=y3,n=n))

update(model, 10000, progress.bar="none"); # Burn-in for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta1","beta2","beta3","beta4","sigma1","sigma2","sigma3","sigma4","sigma5" ), 
                     n.iter=20000, progress.bar="text")

summary(samp)

saveRDS(samp,"modelrun.rds")

traceplot(samp)

# sometimes the gelman plot won't fit on a screen
# we havve to reduce the margins
par(mar=c(.4,.4,.4,.4))
gelman.plot(samp)
gelman.diag(samp)
densplot(samp)
acfplot(samp)

# get the effective sample size
effectiveSize(samp)

sampmatrix = as.matrix(samp)

# take the first four parameters, but drop off the noise!
param <-sampmatrix[sample(nrow(sampmatrix),size = 100, replace=FALSE),1:4]

# create a design matrix
n <- nrow(df_jags_year)
# make an intercept
intercept <- rep(1,n)
# bind your data together in the order used for modelling
data <- cbind(intercept,y1,y2,y3)
pred <- param %*% t(data)
pred <- t(pred)
# replicate the results
act <- rep(x,100)
par(mar=c(1,1,1,1))
dev.off()
plot(act,pred,xlab="Actual",ylab="Predicted")
