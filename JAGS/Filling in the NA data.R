#selecting data from wdi
colnames(our_indicator_data) = c("Country.Code","Region","Country.Name","Indicator.Code","Indicator.Name","Year", "value")
var_correlation = c("SP.DYN.LE00.IN","SN.ITK.DEFC.ZS","SH.DYN.MORT")

#subset the data with only the world and the two indicators above about population and people in poverty in %
correlation_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_correlation)
correlation_data = subset(correlation_data, select = c("Indicator.Code","Country.Name", "Country.Code", "Year", "value"))
correlation_data = dcast(correlation_data,Country.Code + Country.Name + Year ~ Indicator.Code, value.var ="value")
df_correlation_year = subset(correlation_data, correlation_data$Year==2012)
df_correlation_year = df_correlation_year[-which(rowSums(is.na(df_correlation_year))> 0),]

life_expacteny2 = df_correlation_year$SP.DYN.LE00.IN
undernourishment2 = df_correlation_year$SN.ITK.DEFC.ZS
child_mortality2 = df_correlation_year$SH.DYN.MORT
n2 <- nrow(df_correlation_year)

y21 <- life_expacteny
y22 <- undernourishment
y23 <- child_mortality

stand_y21 <- (y21-mean(y21))/sd(y21)
stand_y22 <- (y22-mean(y22))/sd(y22)
stand_y23 <- (y23-mean(y23))/sd(y23)

# make an intercept
intercept2 <- rep(1,n)
# bind your data together in the order used for modelling
data2 <- cbind(intercept2,stand_y21,stand_y22,standy_23)
pred2 <- param %*% t(data)
pred2 <- t(pred)
# replicate the results
act2 <- rep(x,100)
par(mar=c(1,1,1,1))
dev.off()
plot(pred2)