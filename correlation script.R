cor(x, y,  method = "pearson", use = "complete.obs")

var_correlation = c("SI.POV.DDAY","SP.DYN.LE00.IN","SN.ITK.DEFC.ZS","SH.DYN.MORT")
colnames(our_indicator_data) = c("Country.Code","Region","Country.Name","Indicator.Code","Indicator.Name","Year", "value")

#subset the data with only the world and the two indicators above about population and people in poverty in %
jags_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_correlation)
jags_data = subset(jags_data, select = c("Indicator.Code","Country.Name", "Country.Code", "Year", "value"))
df_correlation = dcast(jags_data,Country.Code + Country.Name + Year ~ Indicator.Code, value.var ="value")
df_correlation = subset(df_correlation, df_correlation$Year==2012)

i = df_correlation$"SP.DYN.IMRT.IN"

cor(df_correlation$SI.POV.DDAY,i ,method = "pearson", use = "complete.obs")

list("SH.DYN.MORT","SP.DYN.IMRT.IN","SN.ITK.DEFC.ZS"