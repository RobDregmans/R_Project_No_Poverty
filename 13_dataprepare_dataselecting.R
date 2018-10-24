library(reshape)
library(reshape2)


# Read the data in correctly
mydata_utf8 = read.table("data/WDI.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE)  
mydat_sdg = read.table("data/sdg/SDGData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE) 

# drop off the final blank column
mydata = mydata_utf8[,1:62]

dim(mydata)
all_code = unique(mydata[,1:2])
all_var = unique(mydata[,3:4])

# use the right countries
country_code = read.table("data/country_code.csv",sep=",",header=FALSE)  
# listing all our variables and group them by UN targets or characteristics

Grouping_variables_population = list("SP.RUR.TOTL.ZS","SP.POP.GROW","SP.POP.TOTL","SP.POP.TOTL.MA.IN","SP.POP.TOTL.FE.IN")

Target_1_Headcount_Poverty_190_320_550 = list("SI.POV.DDAY","SI.POV.LMIC,SI.POV.UMIC","SI.POV.GAPS,SI.POV.LMIC.GP",
                                              "SI.POV.UMIC.GP")

Target_2_National_Poverty_Lines_Indicator = list("SI.POV.NAHC","SI.POV.RUHC","SI.POV.URHC","SI.POV.NAGP","SI.POV.RUGP",
                                              "SI.POV.URGP")

Target_3_A_Social_insurance_programm = list("per_si_allsi.cov_pop_tot",
                                         "per_si_allsi.cov_q2_tot",
                                         "per_si_allsi.cov_q3_tot",
                                         "per_si_allsi.cov_q4_tot",
                                         "per_si_allsi.cov_q1_tot",
                                         "per_si_allsi.cov_q5_tot",
                                         "per_allsp.cov_pop_tot")

Target_3_B_Social_Safety_Net_programm = list("per_sa_allsa.cov_pop_tot",
                                          "per_sa_allsa.cov_q2_tot",
                                          "per_sa_allsa.cov_q3_tot",
                                          "per_sa_allsa.cov_q4_tot",
                                          "per_sa_allsa.cov_q1_tot",
                                          "per_sa_allsa.cov_q5_tot")

Target_3_C_Social_Safety_Net_programm = list("per_lm_alllm.cov_pop_tot",
                                          "per_lm_alllm.cov_q2_tot",
                                          "per_lm_alllm.cov_q3_tot",
                                          "per_lm_alllm.cov_q4_tot",
                                          "per_lm_alllm.cov_q1_tot",
                                          "per_lm_alllm.cov_q5_tot")

indicators_income_share = list("SI.DST.04TH.20","SI.DST.10TH.10","SI.DST.05TH.20","SI.DST.FRST.10",
                            "SI.DST.FRST.20","SI.DST.02ND.20","SI.DST.03RD.20")

indicators_economy = list("NY.GNP.PCAP.CD","NY.GNP.MKTP.CN","FP.CPI.TOTL.ZG","NY.GDP.DEFL.KD.ZG","NY.GDP.DEFL.KD.ZG.AD",
                       "TX.VAL.MRCH.CD.WT","TX.VAL.MRCH.WL.CD","TM.VAL.MRCH.CD.WT","TM.VAL.MRCH.WL.CD","SI.POV.GINI")

indicators_life_span = list("SH.DTH.IMRT","SP.DTH.INFR.ZS","SP.DTH.INFR.ZS","SP.DYN.TO65.FE.ZS","SP.DYN.TO65.MA.ZS",
                         "SP.DYN.IMRT.IN","SP.DYN.IMRT.FE.IN","SP.DYN.IMRT.MA.IN")

#putting all indicators names in one list to merge all lists with a loop, see below
List_of_indicators_list = list(indicators_life_span,indicators_economy,indicators_income_share, Target_3_C_Social_Safety_Net_programm,Target_3_B_Social_Safety_Net_programm,Target_3_A_Social_insurance_programm,Target_2_National_Poverty_Lines_Indicator,Target_1_Headcount_Poverty_190_320_550,Grouping_variables_population)

my_vars = list()
for (i in List_of_indicators_list ) {
  my_vars = append(my_vars,i)
}
my_vars_c = c(my_vars)

my_values = list()
for (i in range(28)) {
  x = (1990+i)
  my_values = append(my_values, x)
}
my_values_c = c(my_values)

# subset the data - selected var with all years
country_data = subset(mydata,mydata$Country.Code %in% country_code$V1)
indicator_data = subset(country_data,country_data$Indicator.Code %in% my_vars_c)

# subset the data - selected var with only 1990 - 2015 & prepare data for plotting
indicator_data_1990_2015 = indicator_data[,c(2:3,4:4,35:60)]
sub("X", "", colnames(indicator_data_1990_2015),drop= TRUE)

indicator_data_melt = melt(indicator_data_1990_2015, id=c("Indicator.Code","Indicator.Name","Country.Code"))

combined_cast=cast(combined_melt, Country  ~ Indicator, value = 'Value')

#bbdata <- na.omit(combined_cast) 


#selecting individual indicators values for one year





#main_idx <- match(c("Country.Code","Indicator.Code","X2013"), names(mydata))
urban_idx <- match(c("Country.Code","Indicator.Code","X2010"), names(mydata))
#indicator_data = indicator_data[,main_idx]

#names(indicator_data) <- c("Country","Indicator","Value")

urban_data = subset(country_data,country_data$Indicator.Code %in% "AG.LND.TOTL.UR.K2")
urban_data = urban_data[,urban_idx]

names(urban_data) <- c("Country","Indicator","Value")
#combined_data = rbind(indicator_data,urban_data)

#combined_melt = melt(combined_data, id=c("Country","Indicator","Value"))
#combined_cast=cast(combined_melt, Country  ~ Indicator, value = 'Value')

#bbdata <- na.omit(combined_cast) 
#dim(bbdata)
#names(bbdata) <- c("country","agri_land","urban","rail","bbnd","gdp","pop")
#row.names(bbdata) <- 1:nrow(bbdata)

#plot(bbdata$gdp,bbdata$bbnd,log='xy')
#write.csv(bbdata,"data/bbdata.csv")

