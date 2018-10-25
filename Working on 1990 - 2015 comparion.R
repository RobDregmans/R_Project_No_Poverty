library(plotly)
library(tidyr)
df %>% drop_na()
#get subtabel for poverty headcount 1.90 from main tabel
poverty_1.90= subset(our_indicator_data, our_indicator_data$'Indicator Code' == 'SI.POV.DDAY', select = c("Country Code","Region","Year","Value","Region/Country Name"))
colnames(poverty_1.90) = c("Country Code", "Region", "Year","Poverty Headcount 1.90","Region/Country Name")

poverty_1.90_per_region = subset(poverty_1.90,poverty_1.90$`Region/Country Name`%in% region_list)
poverty_1.90_per_region = poverty_1.90_per_region[,-c(2)]

poverty_1.90_per_region_na = poverty_1.90_per_region %>% drop_na()

# plot the poverty headcount idexes of those countries over time
x <- list( title = "Years" )
y <- list( title = "Poverty Headcount" )
m <- list(l=150, r=20, b=70, t=10)
pline <- plot_ly(poverty_1.90_per_region_na, x = ~poverty_1.90_per_region_na$Year, y = ~poverty_1.90_per_region_na$`Poverty Headcount 1.90`, color = ~poverty_1.90_per_region_na$`Country Code`) %>%
  add_lines(line = list(shape = "spline"))  %>% layout(xaxis = x, yaxis = y, margin=m)
pline


