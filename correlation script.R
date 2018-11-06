var_correlation = c("SI.POV.DDAY","SP.DYN.LE00.IN","SN.ITK.DEFC.ZS","SH.DYN.MORT")

#subset the data with only the world and the two indicators above about population and people in poverty in %
df_correlation = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_correlation)
df_correlation = subset(df_correlation, select = c("Indicator.Code", "Country.Code", "Year", "Value"))
df_correlation = dcast(df_correlation,Country.Code + Year ~ Indicator.Code, value.var ="Value")
df_correlation = subset(df_correlation, df_correlation$Year==2012)
df_correlation = na.omit(df_correlation)
df_correlation = df_correlation[,c(3:6)]

i = df_correlation$"SP.DYN.IMRT.IN"

cor_table <- cor(df_correlation, method = "pearson")
cor_table = round(cor_table,2)

install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")

chart.Correlation(df_correlation, histogram=TRUE, pch=19)


#In the  plot, the distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)
