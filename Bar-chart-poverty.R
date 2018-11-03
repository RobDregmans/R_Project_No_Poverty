#Urban population (% of total) SP.URB.TOTL.IN.ZS
#Urban poverty headcount ratio at national poverty lines (% of urban population) SI.POV.URHC
#Rural population (% of total population) SP.RUR.TOTL.ZS
#Rural poverty headcount ratio at national poverty lines (% of rural population) SI.POV.RUHC

WDI = read.table("WDIData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE)  
new_data = read.table("CountryTable_1.9.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE) 
WDI = WDI[,1:62]
colnames(WDI) <- sub("X", "", colnames(WDI))
WDI <- WDI[,c(2:3,4:4,50:60)]
all_var = unique(WDI[,2:3])
pop = c("SP.POP.TOTL")
WDI_pop = subset(WDI,WDI$Indicator.Code %in% pop)
merged = merge(WDI,new_data,id = "Country.Code")

var_rural_urban = c("SP.URB.TOTL.IN.ZS", "SI.POV.URHC", "SP.RUR.TOTL.ZS", "SI.POV.RUHC")
newvar = c("SI.POV.DDAY", "SP.POP.TOTL")
world = c("WLD")

library(reshape)
library(reshape2)
library(zoo)
library(plotly)

# subset the data - selected var with all years

bar_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% newvar)
bar_data = subset(bar_data,bar_data$'Country.Code' %in% world)

wide = dcast(bar_data, Indicator.Name + Country.Code + variable ~ Indicator.Code,  value.var = "value" , fun.aggregate = mean, na.rm = T)

wide$SP.POP.TOTL <- na.locf(wide$SP.POP.TOTL)
wide <- na.omit(wide)
wide$SI.POV.DDAY <- wide$SI.POV.DDAY/100
wide$absolute <- wide$SI.POV.DDAY * wide$SP.POP.TOTL

p <- plot_ly(wide, x = wide$variable, y = ~wide$absolute, type = 'scatter', mode = 'lines')
p





