library(plotly)
#layout
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
#plot the graph
plot_ly(wide, x = ~variable, y = ~absolute, type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = a, yaxis = b)

