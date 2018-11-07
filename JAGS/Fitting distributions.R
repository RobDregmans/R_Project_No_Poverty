library(MASS)
library(fitdistrplus)



fit1 <- fitdistr(df_jags_year$`df_mean$Mean`, densfun="exponential")  # we assume my_data ~ Normal(?,?)
fit2 <- fitdistr(df_jags_year$SH.DYN.MORT`, densfun="exponential")  # we assume my_data ~ Normal(?,?)
fit3 <- fitdistr(df_jags_year$SP.DYN.LE00.IN, densfun="Weibull")  # we assume my_data ~ Normal(?,?)
fit4 <- fitdistr(df_jags_year$SN.ITK.DEFC.ZS, densfun="exponential")  # we assume my_data ~ Normal(?,?)

fit3 <- fitdistr(df_jags_year$SH.DYN.MORT, densfun="exponential")  # we assume my_data ~ Normal(?,?)

hist(df_jags_year$SN.ITK.DEFC.ZS, pch=20, breaks=15, prob=TRUE, main="")
curve(dexp(x,0.082930,log = FALSE), col="red", lwd=2, add=


curve(dexp(x, fit3$estimate[1],log = FALSE), col="red", lwd=2, add=T)
curve(dexp(x, 0.07209, log = FALSE), col="red", lwd=2, add=T)



curve(dweibull(x, 14, 74.74,log = FALSE), col="red", lwd=2, add=T)


dno

