#https://github.com/worldbank/sdgatlas2018/blob/master/INSTALL.md look here for more instructions
# YOU ALSO NEED THE Style.R to make graphs, see our github
#install World Bank packages
install.packages(c("countrycode","ggalluvial","ggmosaic","pdftools","png","proj4"  ,"readstata13","rgdal", "OECD"))
devtools::install_github("worldbank/wbgviz", subdir = "wbgdata")
devtools::install_github("worldbank/wbgviz", subdir = "wbgcharts")
devtools::install_github("worldbank/wbgviz", subdir = "wbgmaps")
devtools::install_github("worldbank/wbgviz", subdir = "wbggeo")

devtools::install_version("treemapify", version = "2.4.0")
devtools::install_github("econandrew/ggtreemap")

#extrafont::font_import(system.file("fonts", package = "wbgcharts"))


fig_sdg2_undernourish_time <- function(years = 1991:2016) {
  indicator <- "SN.ITK.DEFC.ZS"
  
  df <- wbgdata(c("WLD", wbgref$regions$iso3c),
    indicator, years = years, indicator.wide = FALSE, removeNA = TRUE
    # Comment the next two lines to use live API data
    #offline = "only",
    #offline.file = "inputs/cached_api_data/fig_sdg2_undernourish_time.csv"
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), aspect_ratio = 3){
      ggplot(df, aes(x = date, y = value, group = iso3c, color = iso3c, linetype = iso3c)) +
        geom_line(size = style$linesize) +
        scale_x_continuous(breaks = bracketed_breaks(at = 5)) +
        scale_y_continuous(limits = c(0, 30)) +
        scale_colour_manual(
          labels = wbgref$all_geo$labels,
          values = c(style$colors$regions, style$colors$world)
        ) +
        scale_linetype_manual(
          labels = wbgref$all_geo$labels,
          values = c(style$linetypes$regions, style$linetypes$world)
        ) +
        style$theme() +
        style$theme_legend("top")
    },
    title = "Undernourishment declining in almost every region, remains highest in Sub-Saharan Africa and South Asia.",
    subtitle = wbg_name(indicator)
  )
}
