library(reshape)
library(reshape2)
library(plotly)
library(dplyr)
library(tidyr)

world = c("WLD")
var_i = c("SI.POV.DDAY", "SP.POP.TOTL")
#subset the data with only the world and the two indicators above about population and people in poverty in %
graph_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_i)
graph_data = subset(graph_data,graph_data$'Country.Code' %in% world)
#split the indicator column into two columns
wide = dcast(graph_data, Indicator.Name + Country.Code + variable ~ Indicator.Code,  value.var = "value" , fun.aggregate = mean, na.rm = T)
#this is a workaround for getting for each year the value for the two indicators into the same row
new1 <- wide[order(wide$SP.POP.TOTL),]
pop <- new1[1:26,]
new2 <- wide[order(wide$SI.POV.DDAY),]
si<-subset(new2,new2$SI.POV.DDAY != "NaN") 
si2 <-subset(si, select=c("SI.POV.DDAY", "variable"))
pop <- subset(pop, select = -SI.POV.DDAY )
wide = merge(pop,si2,id = "variable")
#omit all na values
wide <- na.omit(wide)
#divide the percentage indicator by 100 to get it scaled between 0 and 1
wide$SI.POV.DDAY <- wide$SI.POV.DDAY/100
#multiply the data to get the absolute number of people living in poverty
wide$absolute <- wide$SI.POV.DDAY * wide$SP.POP.TOTL
#include layout
a <- list(
  title = "Years",
  showticklabels = TRUE,
  gridwidth = 2,
  autotick = F, 
  dtick = 2,
  range = c(0,25)
)
b <- list(
  title = "Number of people living in poverty (1,90$ a day)",
  showticklabels = TRUE,
  gridwidth = 2,
  range = c(-5000000,2000000000),
  showline = T
)
#plot the data
plot_ly(wide, x = ~variable, y = ~absolute, type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = a, yaxis = b)

