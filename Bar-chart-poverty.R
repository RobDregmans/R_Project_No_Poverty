#Urban population (% of total) SP.URB.TOTL.IN.ZS
#Urban poverty headcount ratio at national poverty lines (% of urban population) SI.POV.URHC
#Rural population (% of total population) SP.RUR.TOTL.ZS
#Rural poverty headcount ratio at national poverty lines (% of rural population) SI.POV.RUHC
#var_rural_urban = c("SP.URB.TOTL.IN.ZS", "SI.POV.URHC", "SP.RUR.TOTL.ZS", "SI.POV.RUHC")

library(reshape)
library(reshape2)
library(zoo)
library(plotly)
world = c("WLD")
graph_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% newvar)
graph_data = subset(graph_data,graph_data$'Country.Code' %in% world)
wide = dcast(graph_data, Indicator.Name + Country.Code + variable ~ Indicator.Code,  value.var = "value" , fun.aggregate = mean, na.rm = T)
wide$SP.POP.TOTL <- na.locf(wide$SP.POP.TOTL)
r <- do.call(rbind, by(wide, wide$SP.POP.TOTL, na.locf))
wide <- na.omit(wide)
wide$SI.POV.DDAY <- wide$SI.POV.DDAY/100
wide$absolute <- wide$SI.POV.DDAY * wide$SP.POP.TOTL
plot_ly(wide, x = wide$variable, y = ~wide$absolute, type = 'scatter', mode = 'lines')


#probeersel
new1 <- wide[order(wide$SP.POP.TOTL),]
pop <- new1[1:36,]

new2 <- wide[order("SI.POV.DDAY", "Indicator.Name"),]
si <- new1[1:15,]
merge1 <- si[,c("SI.POV.DDAY", "variable")]

