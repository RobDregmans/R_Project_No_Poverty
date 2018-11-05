library(plotly)
library(ggplot2)

#making bubble chart of poverty 3.10 and life expectancy
colnames(our_indicator_data) = c("Country.Code","Region","Country.Name","Indicator.Code","Indicator.Name","Year", "value")
var_i = c("SE.XPD.TOTL.GD.ZS","SI.POV.DDAY","SP.POP.TOTL")


#subset the data with only the world and the two indicators above about population and people in poverty and life expectancy
graph_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_i)
test <- subset(graph_data, select = c("Indicator.Code","Region", "Country.Name", "Country.Code", "Year", "value"))
df_bubble = dcast(test,Country.Code + Country.Name + Region + Year ~ Indicator.Code, value.var ="value")
df_bubble_2013 = df_bubble[which(df_bubble$Year == 2012),]
df_bubble_2013$SP.POP.TOTL = df_bubble_2013$SP.POP.TOTL/1000000
df_bubble_2013$SI.POV.DDAY = df_bubble_2013$SI.POV.DDAY/100
df_bubble_2013$TotalPopPov = df_bubble_2013$SI.POV.DDAY * df_bubble_2013$SP.POP.TOTL

colnames(df_bubble_2013) = c("Country.Code", "Country.Name","Region","Year","EduSpending", "Poverty","TotalPop","TotalPopPov")

#omit NA's
df_bubble_2013_na = na.omit(df_bubble_2013)
#order to omit regions, only hold countries
df_bubble_2013_na = df_bubble_2013_na[order(df_bubble_2013_na$Region, df_bubble_2013_na$Country.Name),]
x = as.numeric(dim(df_bubble_2013_na))
k = x[1]
df_bubble_2013_na = df_bubble_2013_na[c(13:k),]
#fix values with to many decimals and poverty percentage value
df_bubble_2013_na$Poverty = df_bubble_2013_na$Poverty *100
df_bubble_2013_na$Poverty = round(df_bubble_2013_na$Poverty,2)
df_bubble_2013_na$TotalPop = round(df_bubble_2013_na$TotalPop,2)
df_bubble_2013_na$TotalPopPov = round(df_bubble_2013_na$TotalPopPov,2)
df_bubble_2013_na$EduSpending = round(df_bubble_2013_na$EduSpending,2)

#determine size per bubble
slope <- 2.8e-6
df_bubble_2013_na$size <- sqrt(df_bubble_2013_na$Poverty * slope)*5000
colors = c('#FF934F','#FFD166','#5FCC8C','#EF6464','#5DCBEF','#8C6ACC','#CC6AAC')



p <- plot_ly(df_bubble_2013_na, x = ~EduSpending, y = ~Poverty, color = ~Region, size = ~size, colors = colors,
             type = 'scatter', mode = 'markers', sizes = c(min(df_bubble_2013_na$size), max(df_bubble_2013_na$size)),
             marker = list(symbol = 'circle', sizemode = 'diameter',
                           line = list(width = 2, color = '#FFFFFF')),
             text = ~paste('Country:', Country.Name,
                           '<br> Region:', Region,
                           '<br>Spending on Education % of GDP:', EduSpending,"%",
                           "<br> Percentage of population living in Extreme Poverty", Poverty,"%",
                           "<br> Total population living in Extreme Poverty:", TotalPopPov,"M",
                           '<br> Total population:', TotalPop, "M")) %>%
  layout(title = 'Public spending on education and the prevalence of Extreme Poverty',
         xaxis = list(title = 'Public spending on education (% of GDP)',
                      gridcolor = 'rgb(255, 255, 255)',
                      zerolinewidth = 1,
                      ticklen = 4,
                      gridwidth = 2),
         yaxis = list(title = 'Prevalence of Poverty (%population)',
                      gridcolor = 'rgb(255, 255, 255)',
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')

p


