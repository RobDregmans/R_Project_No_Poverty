#selecting data from wdi
colnames(our_indicator_data) = c("Country.Code","Region","Country.Name","Indicator.Code","Indicator.Name","Year", "value")
var_i = c("SI.POV.DDAY","SP.DYN.LE00.IN","SE.XPD.TOTL.GD.ZS")
#subset the data with only the world and the two indicators above about population and people in poverty in %
graph_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_i)
test <- subset(graph_data, select = c("Indicator.Code","Country.Name", "Country.Code", "Year", "value"))
df_jags = dcast(test,Country.Code + Country.Name + Year ~ Indicator.Code, value.var ="value")

df_jags_year = subset(df_jags,df_jags$Year == '2015')
df_jags_year = df_jags_year[-which(rowSums(is.na(df_jags_year))> 0),]

poverty = df_jags_year$SI.POV.DDAY
life_expacteny = df_jags_year$SP.DYN.LE00.IN
Education = df_jags_year$SE.XPD.TOTL.GD.ZS
n = nrow(df_jags_year)

x <- poverty
y1 <- life_expacteny
y2 <- Education

library(rjags)

x <- (x-mean(x))/sd(x)
y1 <- (y1-mean(y1))/sd(y1)
y2 <- (y2-mean(y2))/sd(y2)

View(x)

model_string <- "model{

# Likelihood
for(i in 1:n){
x[i]   ~ dnorm(mu[i],inv.var1)
mu[i] <- beta1 + beta2*y1[i] + beta3*y2[i]
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

model <- jags.model(textConnection(model_string), data = list(x=x, y1=y1,y2=y2,n=n))

update(model, 10000, progress.bar="none"); # Burn-in for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta1","beta2","beta3","sigma1","sigma2","sigma3","sigma4" ), 
                     n.iter=20000, progress.bar="text")

summary(samp)
