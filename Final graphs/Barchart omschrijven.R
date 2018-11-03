#PLOT BARCHART
x <= c("low extreme","high extreme"),
y <= c(130000,600000),
text= c()

data <- data.frame(x, y, text)


go.Scatter(
    x=["low extreme","high extreme"],
    y=average_undernourishment,
    name='Percentage',
    mode = "markers",
    marker=dict(line=dict(color=porange, width=10)),
    yaxis = 'y2')]
layout = go.Layout(
  title='Figure 11 <br>Target 2.4: Higher number of undernourished people in countries with climate extremes',
  yaxis=dict(title= 'Absolute', range = [0,700000000], titlefont= dict(color=pblue),tickfont=dict(color=pblue),),
  yaxis2=dict(title='Percentage', showgrid=False, range = [13,16.9], titlefont= dict(color=porange),
              tickfont=dict(color=porange), 
              overlaying='y',
              anchor='free',
              side='right',
              position=1)
)
fig = dict(data=data, layout=layout)
py.iplot(fig, filename='low-extreme-vs-high-extreme')

columns = list('RegionCode','CountryCode', 'CountryName','RequestYear',"HeadCount",'ReqYearPopulation','PPP')


df_poverty_extreme = subset(our_indicator_data,our_indicator_data$'Indicator Code' == "SI.POV.DDAY")
df_poverty_extreme = subset(df_poverty_extreme, df_poverty_extreme$`Country Code` == 'WLD')
df_poverty_1.90_povnet = read.table("data/CountryTable_1.9.csv", header = TRUE, sep = ',')
df_poverty_1.90_povnet = df_poverty_1.90_povnet %>% select(columns)
x <- c('Product A', 'Product B', 'Product C')
y <- c(20, 14, 23)
text <- c('27% market share', '24% market share', '19% market share')
data <- data.frame(x, y, text)

p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
  layout(title = "January 2013 Sales Report",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
