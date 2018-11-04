#making bubble chart of poverty 3.10 and life expectancy
colnames(our_indicator_data) = c("Country.Code","Region","Country.Name","Indicator.Code","Indicator.Name","Year", "value")
var_i = c("SP.DYN.LE00.IN","SI.POV.DDAY","SP.POP.TOTL")


#subset the data with only the world and the two indicators above about population and people in poverty and life expectancy
graph_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_i)
test <- subset(graph_data, select = c("Indicator.Code","Region", "Country.Name", "Country.Code", "Year", "value"))
df_bubble = dcast(test,Country.Code + Country.Name + Region + Year ~ Indicator.Code, value.var ="value")
df_bubble_2013 = df_bubble[which(df_bubble$Year == 2013),]
df_bubble_2013$SP.POP.TOTL = df_bubble_2013$SP.POP.TOTL/1000000
df_bubble_2013$SI.POV.DDAY = df_bubble_2013$SI.POV.DDAY/100
df_bubble_2013$TotalPopPov = df_bubble_2013$SI.POV.DDAY * df_bubble_2013$SP.POP.TOTL
colnames(df_bubble_2013) = c("Country.Code", "Country.Name","Region","Year","Poverty","LifeExpec","TotalPop","TotalPopPov")
df_bubble_2013 = df_bubble_2013[order(df_bubble_2013$Region, df_bubble_2013$Country.Name),]
df_bubble_2013$Poverty = df_bubble_2013$Poverty *100

slope <- 0.50e-5
df_bubble_2013$size <- sqrt(df_bubble_2013$TotalPopPov * slope)*5000
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

df_bubble_2013_na = na.omit(df_bubble_2013)
df_bubble_2013_na = df_bubble_2013_na[c(15:87),]
                                            
                                            
                                            
                                            
p <- plot_ly(df_bubble_2013_na, x = ~LifeExpec, y = ~Poverty, color = ~Region, size = ~size, colors = colors,
             type = 'scatter', mode = 'markers', sizes = c(min(df_bubble_2013_na$size), max(df_bubble_2013_na$size)),
             marker = list(symbol = 'circle', sizemode = 'diameter',
                           line = list(width = 2, color = '#FFFFFF')),
             text = ~paste('Country:', Country.Name, '<br>Life Expectancy:', LifeExpec, 'Region:', Region,'<br>Pop.:',
                           "Total pop. below extreme poverty:", TotalPopPov,
                           '<br>Pop.:', TotalPop)) %>%
  layout(title = '\n Life Expectancy & Extreme Poverty 2013',
         xaxis = list(title = 'Life expectancy in years (by birth)',
                      gridcolor = 'rgb(255, 255, 255)',
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwidth = 2),
         yaxis = list(title = 'Prevalence of Poverty (% of population)',
                      gridcolor = 'rgb(255, 255, 255)',
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')
p
