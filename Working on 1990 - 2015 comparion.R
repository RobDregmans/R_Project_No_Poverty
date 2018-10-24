library(plotly)

p <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar"
)

#get subtabel for poverty headcount 1.90 from main tabel
poverty_1.90= subset(indicator_data_melt, Indicator.Code = SI.POV.DDAY, select = c("Country.Code","variable", "value"))
colnames(poverty_1.90) = c("Country Code", "Year", "Poverty Headcount 1.90")

#get subtabel for poverty headcount 1.90 from main tabel
region_info = read.table("R project/UNSD — Methodology_final.csv", header = TRUE, sep = ";")
colnames(country_code) = c("Country Code")



region_codes = merge(region_info,country_code, id = "country code")
