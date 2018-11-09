library(rjags)
#selecting data from wdi
var_predictors = c("SP.DYN.LE00.IN","SN.ITK.DEFC.ZS","SH.DYN.MORT")

#subset the data with the three predicters
df_predictors = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_predictors)
df_predictors = subset(df_predictors, select = c("Indicator.Code","Region","Country.Name", "Country.Code", "Year", "Value"))
df_predictors = dcast(df_predictors,Country.Code + Region + Country.Name + Year ~ Indicator.Code, value.var ="Value")
df_predictors = subset(df_predictors, df_predictors$Year==2015)
df_predictors = df_predictors[-which(rowSums(is.na(df_predictors))> 0),]
df_predictors = subset(df_predictors, df_predictors$Country.Code %in% all_countries$Country.Code)

#is the script Average poverty 2010 - 2015
#selecting data from wdi + average of 6 years 2010 - 2015 -----------
year = c('2010','2011', '2012','2013','2014','2015')
poverty_average = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% "SI.POV.DDAY")
poverty_average = subset(poverty_average,poverty_average$Year %in% year)
poverty_average = subset(poverty_average, select = c("Indicator.Code","Country.Name", "Country.Code", "Year", "Value"))
#Calculate average over columns
poverty_average = dcast(poverty_average,Country.Code + Country.Name + Indicator.Code ~ Year, value.var ="Value")
poverty_average['Mean'] = rowMeans(poverty_average[,4:9],na.rm = T)
poverty_average = poverty_average[,c(1:2,10:10)]
#dropping zero's and NA's
poverty_average = na.omit(poverty_average)
poverty_average = poverty_average[apply(poverty_average!=0, 1, all),]
### ------------------------------------------------------------------

#combine the poverty dataframe with the predictors dataframe
poverty_JAGS = subset(poverty_average,poverty_average$Country.Code %in% df_predictors$Country.Code)
predictors_JAGS = subset(df_predictors,df_predictors$Country.Code %in% poverty_average$Country.Code)
df_jags_year = cbind(predictors_JAGS,poverty_JAGS$Mean)

#Select the predictors and the predicted value from the dataframe
poverty = df_jags_year$`poverty_JAGS$Mean`
life_expacteny = df_jags_year$SP.DYN.LE00.IN
undernourishment = df_jags_year$SN.ITK.DEFC.ZS
child_mortality = df_jags_year$SH.DYN.MORT
n = nrow(df_jags_year)

#standardize the values
y <- poverty
x1 <- undernourishment
x2 <- child_mortality
x3 <- life_expacteny
y  <- (y-mean(y))/sd(y)
x1 <- (x1-mean(x1))/sd(x1)
x2 <- (x2-mean(x2))/sd(x2)
x3 <- (x3-mean(x3))/sd(x3)

#Our model
model_string <- "model{

# Likelihood
for(i in 1:n){
y[i]  ~ dnorm(mu[i],inv.var1)
mu[i] <- beta1 + beta2*x1[i] + beta3*x2[i] + beta4*x3[i]
}

# Prior for beta
beta1 ~ dexp(0.072029)
beta2 ~ dexp(0.082930)       #undernourishment
beta3 ~ dexp(0.032092)       #child mortality
beta4 ~ dweib(14,74.74)      #life expactency

# Prior for the inverse variance
var1       ~   dunif(0,0.1)  #poverty
inv.var1   <-  1/var1
sigma1     <-  sqrt(var1)
}"

model <- jags.model(textConnection(model_string), data = list(y=y, x1=x1,x2=x2,x3=x3,n=n),n.chains = 4)

update(model, 10000, progress.bar="none"); # Burn-in for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta1","beta2","beta3","beta4","sigma1"), 
                     n.iter=20000, progress.bar="text")

saveRDS(samp,"modelrun.rds")

#summary + plots
summary(samp)
traceplot(samp)
par(mar=c(.4,.4,.4,.4))
gelman.plot(samp)
densplot(samp)
acfplot(samp)
effectiveSize(samp)

###---- Predicting ----------------------------------------------------------

sampmatrix = as.matrix(samp)
# take the first four parameters, but drop off the noise!
param <-sampmatrix[sample(nrow(sampmatrix),size = 100, replace=FALSE),1:4]
# create a design matrix
n <- nrow(df_jags_year)
# make an intercept
intercept <- rep(1,n)
# bind your data together in the order used for modelling
data <- cbind(intercept,x1,x2,x3)
pred <- param %*% t(data)
pred <- t(pred)
# replicate the results
act <- rep(y,100)
par(mar=c(1,1,1,1))
plot(act,pred,xlab="Actual",ylab="Predicted")

### ----- Filling in the NA -------------------------------------------------
#creates list with countries which have NA value on poverty
life_expactency2 = df_predictors$SP.DYN.LE00.IN
undernourishment2 = df_predictors$SN.ITK.DEFC.ZS
child_mortality2 = df_predictors$SH.DYN.MORT
n2 <- nrow(df_predictors)

#standardize these values
x21 <- life_expactency2
x22 <- undernourishment2
x23 <- child_mortality2
x21 <- (x21-mean(x21))/sd(x21)
x22 <- (x22-mean(x22))/sd(x22)
x23 <- (x23-mean(x23))/sd(x23)

# make an intercept
intercept2 <- rep(1,n2)
#predict the values for all the countries within the df_predictors dataset
data2 <- cbind(intercept2,x21,x22,x23)
pred2 <- param %*% t(data2)
pred2 <- t(pred2)
predicted = rowMeans(pred2[,0:100],na.rm = T)

predicted_poverty <- data.frame(
  c1 = df_predictors$Country.Code,
  c2 = df_predictors$Country.Name,
  c3 = predicted
)
names(predicted_poverty) <- c("Country.Code","Country.Name","score")

predicted_poverty$poverty = (predicted_poverty$score * sd(poverty)) + mean(poverty)