library(plotly)
library(tidyr)
df %>% drop_na()
#get subtabel for poverty headcount 1.90 from main tabel
poverty_1.90= subset(our_indicator_data, our_indicator_data$'Indicator Code' == 'SI.POV.DDAY', select = c("Country Code","Region","Year","Value","Region/Country Name"))
colnames(poverty_1.90) = c("Country Code", "Region", "Year","Poverty Headcount 1.90","Region/Country Name")

poverty_1.90_per_region = subset(poverty_1.90,poverty_1.90$`Region/Country Name`%in% region_list)
poverty_1.90_per_region = poverty_1.90_per_region[,-c(2)]

poverty_1.90_per_region_na = poverty_1.90_per_region %>% drop_na()
poverty_1.90_per_region_na$`Poverty Headcount 1.90` = poverty_1.90_per_region_na$`Poverty Headcount 1.90`*10

# plot the poverty headcount idexes of those countries over time
x <- list( title = "Years" )
y <- list( title = "Poverty Headcount" )
m <- list(l=150, r=20, b=70, t=10)
pline <- plot_ly(poverty_1.90_per_region_na, x = ~poverty_1.90_per_region_na$Year, y = ~poverty_1.90_per_region_na$`Poverty Headcount 1.90`, color = ~poverty_1.90_per_region_na$`Country Code`) %>%
  add_lines(line = list(shape = "spline"))  %>% layout(xaxis = x, yaxis = y, margin=m)
pline
k = as.character(regions_used[1,1])

EAS = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[1,1]))))
ECS = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[2,1]))))
LCN = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[3,1]))))
MEA = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[4,1]))))
SAS = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[5,1]))))
SSF = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[6,1]))))
Year_frame = c(unique(poverty_1.90_per_region_na$Year))

data_frame_stacked = rbind(EAS,ECS,LCN,MEA,SAS,SSF,Year_frame)

library(plotly)

data <- t(USPersonalExpenditure)
data <- data.frame("year"=rownames(data), data)

library(plotly)
packageVersion('plotly')

p <- plot_ly(poverty_1.90_per_region_na, x = ~unique(poverty_1.90_per_region_na$Year), y =subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[1,1]))), name = 'Europe & Central Asia', stackgroup = 'one',type = 'scatter', mode = 'none', fillcolor = '#F5FF8D')
  #add_trace(y = ~subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[2,1]))), name = 'Europe & Central Asia', fillcolor = '#50CB86')
  #add_trace(y = ~Medical.and.Health, name = 'Medical and Health', fillcolor = '#4C74C9') %>%
  #add_trace(y = ~Personal.Care, name = 'Personal Care', fillcolor = '#700961') %>%
  #add_trace(y = ~Private.Education, name = 'Private Education', fillcolor = '#312F44') %>%
  layout(title = 'Cummalitive people living in Extreme Poverty per region',
         xaxis = list(title = "",
                      showgrid = FALSE),
         yaxis = list(title = "Total population (in million)",
                      showgrid = FALSE))
p

install.packages('plotly')

library(plotly)

data <- t(USPersonalExpenditure)
data <- data.frame("year"=rownames(data), data)

k <- plot_ly(data, x = ~year, y = ~Food.and.Tobacco, name = 'Food and Tobacco', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D') %>%
  add_trace(y = ~Household.Operation, name = 'Household Operation', fillcolor = '#50CB86') %>%
  add_trace(y = ~Medical.and.Health, name = 'Medical and Health', fillcolor = '#4C74C9') %>%
  add_trace(y = ~Personal.Care, name = 'Personal Care', fillcolor = '#700961') %>%
  add_trace(y = ~Private.Education, name = 'Private Education', fillcolor = '#312F44') %>%
  layout(title = 'United States Personal Expenditures by Categories',
         xaxis = list(title = "",
                      showgrid = FALSE),
         yaxis = list(title = "Proportion from the Total Expenditures",
                      showgrid = FALSE,
                      ticksuffix = '%'))
k