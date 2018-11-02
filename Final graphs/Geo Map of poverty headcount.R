
#https://plot.ly/r/choropleth-maps/ for more information
dp= subset(our_indicator_data, our_indicator_data$'Indicator Code' == 'SI.POV.DDAY', select = c("Country Code","Region","Year","Value","Region/Country Name"))
#due to missing data, calculate average over 6 year period
year = list('2010','2011','2012','2013','2014','2015')
dp = subset(dp, dp$'Year' %in% year, select = c("Country Code", "Region","Value","Region/Country Name"))
colnames(dp) = c("code","region","value","country")

country_names_wdi = country_wdi[1]
colnames(country_names_wdi) = c('code')
                 
dp_sum = by(dp$value, dp$code, sum)
colnames(dp_sum) = c('value')
transform(dp_sum, average = dp_sum$value / 6)
dp_sum = do.call(rbind,as.list(dp_sum))

merge(dp_sum,country_names_wdi)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

x2 <- by(x$Frequency, x$Category, sum)
do.call(rbind,as.list(x2))

dp$hover <- with(dp, paste(country, '<br>', "Region", region))

# specify map projection/options
g <- list(
  resolution = 5,
  showcoastlines = T,
  countrycolor = toRGB("grey"),
  coastlinecolor = toRGB("grey"),
  projection = list(type = 'Mercator')
)


k <- plot_geo(dp) %>%
  add_trace(z = ~value, text = ~hover, color = ~value, colors = 'Reds', text = ~value, locations = ~code, marker = list(line = l,color = 'rgb(255,255,255)')) %>%
  colorbar(title = '% population below extreme poverty') %>%
  layout(
    title = 'Prevalance of Extreme Poverty (below 1.90$ a day)',
    geo = g )
k

