
#https://plot.ly/r/choropleth-maps/ for more information
dp = subset(poverty_1.90, poverty_1.90$'Year' == '2015', select = c("Country Code","Region","Poverty Headcount 1.90","Region/Country Name"))
dp[is.na(dp)] <- 0

dg = subset(our_indicator_data, our_indicator_data$`Indicator Code`=='SI.POV.GINI', select = c("Country Code","Indicator Code","Region", "Value","Region/Country Name"))

colnames(dp) = c("code", "region","value","country_name")


# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

h <- plot_geo(dp) %>%
  add_trace(
    z = ~value, color = ~value, colors = 'Reds',
    text = ~value, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = '% population below extreme poverty') %>%
  layout(
    title = 'Prevelance of Extreme Poverty',
    geo = g
  )
h
