library(reshape)

# Read the data in correctly
mydata_utf8 = read.table("WDIData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE)  

# drop off the final blank column
mydata = mydata_utf8[,1:62]

dim(mydata)
all_code = unique(mydata[,1:2])
all_var = unique(mydata[,3:4])

# use the right countries
country_code = read.table("country_code.csv",sep=",",header=FALSE)  
# find the right variables
my_var = c("IT.NET.BBND","NY.GDP.MKTP.CD","SP.POP.TOTL","AG.LND.TOTL.K2","IS.RRS.TOTL.KM")
# subset the data 
country_data = subset(mydata,mydata$Country.Code %in% country_code$V1)
indicator_data = subset(country_data,country_data$Indicator.Code %in% my_var)

main_idx <- match(c("Country.Code","Indicator.Code","X2013"), names(mydata))
urban_idx <- match(c("Country.Code","Indicator.Code","X2010"), names(mydata))
indicator_data = indicator_data[,main_idx]

names(indicator_data) <- c("Country","Indicator","Value")

urban_data = subset(country_data,country_data$Indicator.Code %in% "AG.LND.TOTL.UR.K2")
urban_data = urban_data[,urban_idx]

names(urban_data) <- c("Country","Indicator","Value")
combined_data = rbind(indicator_data,urban_data)

combined_melt = melt(combined_data, id=c("Country","Indicator","Value"))
combined_cast=cast(combined_melt, Country  ~ Indicator, value = 'Value')

bbdata <- na.omit(combined_cast) 
dim(bbdata)
names(bbdata) <- c("country","agri_land","urban","rail","bbnd","gdp","pop")
row.names(bbdata) <- 1:nrow(bbdata)

plot(bbdata$gdp,bbdata$bbnd,log='xy')
write.csv(bbdata,"bbdata.csv")