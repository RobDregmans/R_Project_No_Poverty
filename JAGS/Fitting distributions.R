library(MASS)
library(fitdistrplus)

#fit paraeters with custom distributions
fit1 <- fitdistr(df_jags_year$`df_mean$Mean`, densfun="Weibull")  # we assume my_data ~ Normal(?,?)
fit3 <- fitdistr(df_jags_year$SP.DYN.LE00.IN, densfun="Weibull")  # we assume my_data ~ Normal(?,?)
fit4 <- fitdistr(df_jags_year$SN.ITK.DEFC.ZS, densfun="exponential")  # we assume my_data ~ Normal(?,?)
fit3 <- fitdistr(df_jags_year$SH.DYN.MORT, densfun="exponential")  # we assume my_data ~ Normal(?,?)

#producing a histogram to check distributions
hist(df_jags_year$`df_mean$Mean`, pch=20, breaks=15, prob=TRUE, main="")

#plotting curves in histogram to check distribution with actual values
curve(dexp(x,0.082930,log = FALSE), col="red", lwd=2, add=T)
curve(dweibull(x, 0.5426,8.36158, log = FALSE), col="red", lwd=2, add=T)
curve(dexp(x, fit3$estimate[1],log = FALSE), col="red", lwd=2, add=T)
curve(dexp(x, 0.07209, log = FALSE), col="red", lwd=2, add=T)
curve(dweibull(x, 14, 74.74,log = FALSE), col="red", lwd=2, add=T)


#fit normal distributions for the RJAGS - normal distribution model
fit1 <- fitdistr(df_jags_year$`df_mean$Mean`, densfun="normal")  # we assume my_data ~ Normal(?,?)
fit2 <- fitdistr(df_jags_year$SP.DYN.LE00.IN, densfun="normal")  # we assume my_data ~ Normal(?,?)
fit3 <- fitdistr(df_jags_year$SN.ITK.DEFC.ZS, densfun="normal")  # we assume my_data ~ Normal(?,?)
fit4 <- fitdistr(df_jags_year$SH.DYN.MORT, densfun="normal")  # we assume my_data ~ Normal(?,?)