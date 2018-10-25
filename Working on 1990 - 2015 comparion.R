#Example how to ggplot with bar chart
library(plotly)

p <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar"
)

#setting up indicator data with country names from python @martijn
country_names = read.table("data/country_names.csv",sep = ",", header = TRUE)
country_wdi = country_names[,c(1:2,5:5)]

colnames(country_wdi) = c("Country.Code","Region","Region/Country Name")
indicator_data_merged = merge(country_wdi,indicator_data_melt,id = "Country.Code")

#get subtabel for poverty headcount 1.90 from main tabel
poverty_1.90= subset(indicator_data_merged, Indicator.Code = SI.POV.DDAY, select = c("Country.Code","variable", "value","Region","Region/Country Name"))
colnames(poverty_1.90) = c("Country Code", "Year", "Poverty Headcount 1.90","Region","Region/Country Name")
