#selecting data from wdi + average of 6 years 2010 - 2015
colnames(our_indicator_data) = c("Country.Code","Region","Country.Name","Indicator.Code","Indicator.Name","Year", "value")
var_i = c("SI.POV.DDAY","SP.POP.TOTL")
#subset the data with only the world and the two indicators above about population and people in poverty in %
graph_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_i)
graph_data = subset(graph_data,graph_data$'Year' %in% year)

test <- subset(graph_data, select = c("Indicator.Code","Country.Name", "Country.Code", "Year", "value"))
df_mean_pov = dcast(test,Country.Code + Country.Name + Indicator.Code ~ Year, value.var ="value")
df_mean_pov['mean'] = rowMeans(df_mean_pov[,4:9],na.rm = T)
df_mean_pov = dcast(df_mean_pov,Country.Code + Country.Name  ~ Indicator.Code, value.var ="mean")
colnames(df_mean_pov) = c("Country.Code","Country.Name","MeanHeadCount","MeanPop")
df_mean_pov$MeanPop = df_mean_pov$MeanPop/1000000
df_mean_pov$MeanHeadCount = df_mean_pov$MeanHeadCount/100
df_mean_pov['MeanPovPop'] = df_mean_pov[,3]*df_mean_pov[,4]
df_mean_pov$MeanHeadCount = df_mean_pov$MeanHeadCount*100
df_mean_pov$MeanPop = round(df_mean_pov$MeanPop,2)
df_mean_pov$MeanHeadCount = round(df_mean_pov$MeanHeadCount,2)
df_mean_pov$MeanPovPop = round(df_mean_pov$MeanPovPop,2)


# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

df_mean_pov$hover <- with(df_mean_pov, paste
                                     ( "Year: 2015",'<br>',
                                       "Percentage under extreme poverty line:",MeanHeadCount,"%" ,'<br>',
                                       "Mean population living in extreme poverty:",MeanPovPop,"M",'<br>',
                                       "Total population:",MeanPop,"M",'<br>', 
                                       "Country:", Country.Name))

# specify map projection/options
g <- list(
  resolution = 5,
  showcoastlines = T,
  countrycolor = toRGB("grey"),
  coastlinecolor = toRGB("grey"),
  projection = list(type = 'Mercator') 
)

k <- plot_geo(df_mean_pov) %>%
  add_trace(z = ~MeanHeadCount,text = ~hover, color = ~MeanHeadCount, colors= 'Reds', 
            locations = ~Country.Code, 
            marker = list(line = l,color = 'rgb(255,255,255)')) %>%
  
  colorbar(title = 'Percentage of total population living extreme poverty') %>%
  layout(
    title = 'Prevalance of Extreme Poverty during 2010 - 2015 (below 1.90$ a day)',
    geo = g )
k


