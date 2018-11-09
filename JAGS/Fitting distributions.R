library(MASS)
library(fitdistrplus)

#Fit undernourishment
fit1 <- fitdistr(df_jags_year$SN.ITK.DEFC.ZS, densfun="exponential")  # we assume my_data ~ Normal(?,?)
hist(df_jags_year$SN.ITK.DEFC.ZS, pch=20, breaks=15, prob=TRUE, main="")
curve(dexp(x,0.082936680,log = FALSE), col="red", lwd=2, add=T)

#Fit life expectancy
fit2 <- fitdistr(df_jags_year$SP.DYN.LE00.IN, densfun="Weibull")  # we assume my_data ~ Normal(?,?)
hist(df_jags_year$SP.DYN.LE00.IN, pch=20, breaks=15, prob=TRUE, main="")
curve(dweibull(x,14.74,74.7498634,log = FALSE), col="red", lwd=2, add=T)

#Fit child mortality
fit3 <- fitdistr(df_jags_year$SH.DYN.MORT, densfun="exponential")  # we assume my_data ~ Normal(?,?)
hist(df_jags_year$SH.DYN.MORT, pch=20, breaks=15, prob=TRUE, main="")
curve(dexp(x,0.031210986,log = FALSE), col="red", lwd=2, add=T)

