library(reshape)
library(reshape2)

mydata = read.table("WDI/WDIData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE)  
mydata = mydata[,1:62]

dim(mydata)
all_code = unique(mydata[,1:2])
all_var = unique(mydata[,3:4])

List_jags = list("SI.POV.NAGP","per_si_allsi.cov_pop_tot","per_lm_alllm.cov_pop_tot")

#Social_insurance_programm_1 = list("per_si_allsi.cov_pop_tot")
#Social_Safety_Net_programm_3 = list("per_lm_alllm.cov_pop_tot")

#putting all indicators names in one list to merge all lists with a loop, see below
#List_of_indicators_list = list(Poverty_Lines_Indicators,Social_insurance_programm_1,Social_Safety_Net_programm_2,Social_Safety_Net_programm_3)

my_vars = list()
for (i in List_jags ) {
  my_vars = append(my_vars,i)
}
my_vars_c = c(my_vars)

mydata = subset(mydata,mydata$Indicator.Code %in% my_vars_c)

# subset the data - selected var with only 1990 - 2015 & prepare data for plotting
mydata = mydata[,c(2:3,4:4,35:60)]
colnames(mydata) <- sub("X", "", colnames(mydata))

mydata= melt(mydata, id=c("Indicator.Code","Indicator.Name","Country.Code"), value.name = "Values")

mydata = subset(mydata,mydata$variable == '2012')
mydataNA = mydata[-which(rowSums(is.na(mydata))> 0),]

poverty_line = subset(mydata,mydata$Indicator.Code == "SI.POV.NAGP")
social_insurance = subset(mydata,mydata$Indicator.Code == "per_si_allsi.cov_pop_tot")
Coverage_of_unemployment_benefits = subset(mydata,mydata$Indicator.Code == "per_lm_alllm.cov_pop_tot")

x = poverty_line$Values
y1 = social_insurance$Values
y2 = Coverage_of_unemployment_benefits$Values
View(x)
z = merge(poverty_line$Values,social_insurance$Values)
z = z[-which(rowSums(is.na(z))> 1),]
n <- nrow(poverty_line)

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

update(model, 1000, progress.bar="none"); # Burn-in for 10000 samples

samp <- coda.samples(model, 
                     variable.names=c("beta1","beta2","beta3","beta4","sigma1","sigma2","sigma3","sigma4" ), 
                     n.iter=20000, progress.bar="text")

summary(samp)