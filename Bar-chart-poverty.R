#Urban population (% of total) SP.URB.TOTL.IN.ZS
#Urban poverty headcount ratio at national poverty lines (% of urban population) SI.POV.URHC
#Rural population (% of total population) SP.RUR.TOTL.ZS
#Rural poverty headcount ratio at national poverty lines (% of rural population) SI.POV.RUHC

var_rural_urban = c("SP.URB.TOTL.IN.ZS", "SI.POV.URHC", "SP.RUR.TOTL.ZS", "SI.POV.RUHC")

library(reshape)
library(reshape2)

# subset the data - selected var with all years

indicator_data = subset(mydata,mydata$Indicator.Code %in% var_rural_urban)

# subset the data - selected var with only 1990 - 2015 & prepare data for plotting
indicator_data_1990_2015 = indicator_data[,c(2:3,4:4,35:60)]
colnames(indicator_data_1990_2015) <- sub("X", "", colnames(indicator_data_1990_2015))

temp = dcast(bar_data, `Indicator Name` + `Country Code` + Region + `Region/Country Name` ~ `Indicator Code`, value.var = "Value", fun.aggregate = mean, nar.rm = T)

