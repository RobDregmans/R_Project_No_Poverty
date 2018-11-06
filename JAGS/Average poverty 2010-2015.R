#script calculates the average POV for all countries in the WDI dataset. 
#If a country is not present in the output, this means that the past 6 years no poverty data was submitted

#selecting data from wdi + average of 6 years 2010 - 2015
year = c('2010','2011', '2012','2013','2014','2015')
R_jags_average = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% "SI.POV.DDAY")
R_jags_average = subset(R_jags_average,R_jags_average$Year %in% year)
R_jags_average = subset(R_jags_average, select = c("Indicator.Code","Country.Name", "Country.Code", "Year", "Value"))

#Calculate average over columns
R_jags_average = dcast(R_jags_average,Country.Code + Country.Name + Indicator.Code ~ Year, value.var ="Value")
R_jags_average['Mean'] = rowMeans(R_jags_average[,4:9],na.rm = T)
R_jags_average = R_jags_average[,c(1:2,10:10)]

#dropping zero's and NA's
R_jags_average = na.omit(R_jags_average)
R_jags_average = R_jags_average[apply(R_jags_average!=0, 1, all),]