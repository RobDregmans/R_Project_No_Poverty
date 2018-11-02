library(plotly)
library(tidyr)
#get subtabel for poverty headcount 1.90 from main tabel
poverty_1.90= subset(our_indicator_data, our_indicator_data$'Indicator Code' == 'SI.POV.DDAY', select = c("Country Code","Region","Year","Value","Region/Country Name"))
colnames(poverty_1.90) = c("Country Code", "Region", "Year","Poverty Headcount 1.90","Region/Country Name")

poverty_1.90_per_region = subset(poverty_1.90,poverty_1.90$`Region/Country Name`%in% region_list)
poverty_1.90_per_region = poverty_1.90_per_region[,-c(2)]

poverty_1.90_per_region_na = poverty_1.90_per_region %>% drop_na()
poverty_1.90_per_region_na$`Poverty Headcount 1.90` = poverty_1.90_per_region_na$`Poverty Headcount 1.90`*10

# plot the poverty headcount idexes of those countries over time
xaxis <- list(title = "Year",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))

yaxis <- list(title = "People under poverty line (millions)",
              showgrid = TRUE,
              zeroline = FALSE,
              showline = TRUE,
              showticklabels = TRUE)


m <- list(l=150, r=20, b=70, t=10)


pline <- plot_ly(poverty_1.90_per_region_na, x = ~poverty_1.90_per_region_na$Year, y = ~poverty_1.90_per_region_na$`Poverty Headcount 1.90`, color = ~poverty_1.90_per_region_na$`Country Code`, colors = "Set1") %>%
  add_lines(line = list(shape = "markers",width = 4) )  %>% 
  layout(title = "People living in extreme poverty per region", xaxis = xaxis, yaxis = yaxis,
         autosize = TRUE,
         showlegend = TRUE)
pline


k = as.character(regions_used[1,1])

EAS = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[1,1]))))
ECS = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[2,1]))))
LCN = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[3,1]))))
MEA = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[4,1]))))
SAS = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[5,1]))))
SSF = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[6,1]))))
SSF = c(subset(poverty_1.90_per_region_na$`Poverty Headcount 1.90`,poverty_1.90_per_region_na$`Country Code`== (k = as.character(regions_used[6,1]))))
Y = unique(as.character(poverty_1.90_per_region_na$Year))
poverty_headcount_stacked = cbind(EAS,ECS,LCN,MEA,SAS,SSF,Y)
colnames(poverty_headcount_stacked) =  c(EAS,ECS,LCN,MEA,SAS,SSF,Year)

#data_frame_stacked = rbind(EAS,ECS,LCN,MEA,SAS,SSF,Year_frame)

data.set <- data.frame(
  Time = c(rep(1, 4),rep(2, 4), rep(3, 4), rep(4, 4)),
  Type = rep(c('a', 'b', 'c', 'd'), 4),
  Value = rpois(16, 10)
)

colnames(poverty_1.90_per_region_na) = c("Country","Year","Value","Region") 
poverty_1.90_per_region_na <- poverty_1.90_per_region_na[, c(1,2,4,3)]


ggplot(poverty_1.90_per_region_na, aes(Year,Value)) + geo_line(position = "stack",aes(colour = Region))

ggplot(poverty_1.90_per_region_na, aes(Year, Value)) + geom_area(aes(fill = Region))
