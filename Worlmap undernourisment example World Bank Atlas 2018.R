#https://github.com/worldbank/sdgatlas2018/blob/master/INSTALL.md look here for more instructions
# YOU ALSO NEED THE Style.R to make graphs, see our github
install.packages(c("countrycode","ggalluvial","ggmosaic","pdftools","png","proj4"  ,"readstata13","rgdal", "OECD"))
devtools::install_github("worldbank/wbgviz", subdir = "wbgdata")
devtools::install_github("worldbank/wbgviz", subdir = "wbgcharts")
devtools::install_github("worldbank/wbgviz", subdir = "wbgmaps")
devtools::install_github("worldbank/wbgviz", subdir = "wbggeo")

devtools::install_version("treemapify", version = "2.4.0")
devtools::install_github("econandrew/ggtreemap")

extrafont::font_import(system.file("fonts", package = "wbgcharts"))

library(wbgdata)
library(wbgcharts)
library(wbggeo)
library(wbgmaps)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(gtable)
library(stringr)
source("Styles.R")

fig_sdg2_undernourish_map <- function(year = 2015) {
  indicator <- "SN.ITK.DEFC.ZS"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year
    # Comment the next two lines to use live API data
    #offline = "only",
    #offline.file = "inputs/cached_api_data/fig_sdg2_undernourish_map.csv"
  )
  
  df$bins <- supercut(df$SN.ITK.DEFC.ZS, c(
    "0–5" = "[0,5)",
    "5–15" = "[5,15)",
    "15 and over" = "[15,Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "high") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.2,
    title = "Globally, 1 in 10 people are undernourished and do not have enough food to meet their dietary needs. Undernourishment is most widespread in Sub-Saharan Africa, South Asia, and East Asia & Pacific.",
    subtitle = wbg_name(indicatorID = indicator, year = year),
    source = "Source: Food and Agriculture Organization. World Development Indicators (SN.ITK.DEFC.ZS)."
  )
}