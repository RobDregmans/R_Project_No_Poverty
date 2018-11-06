library(reshape)
library(reshape2)
library(dplyr)
library(tidyr)

# Read the data in correctly
mydata_utf8 = read.table("WDI/WDIData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE)  

# drop off the final blank column
mydata = mydata_utf8[,1:62]

dim(mydata)
all_code = unique(mydata[,1:2])
all_var = unique(mydata[,3:4])

# use the right countries
#country_code = read.table("data/WDI/country_code.csv",sep=",",header=FALSE)  
# listing all our variables and group them by UN targets or characteristics

Grouping_variables_population = list("SE.XPD.TOTL.GD.ZS","P.DYN.LE00.IN","SI.POV.NAGP","SP.DYN.LE00.IN","SE.XPD.TOTL.GD.ZS","SP.RUR.TOTL.ZS","SP.POP.GROW","SP.POP.TOTL","SP.POP.TOTL.MA.IN","SP.POP.TOTL.FE.IN","SP.URB.TOTL.IN.ZS", "SI.POV.URHC", "SP.RUR.TOTL.ZS", "SI.POV.RUHC", "SI.POV.DDAY", "SP.POP.TOTL")


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


# subset the data - selected var with all years
#country_data = subset(mydata,mydata$Country.Code %in% country_code$V1)
indicator_data = subset(mydata,mydata$Indicator.Code %in% my_vars_c)

# subset the data - selected var with only 1990 - 2015 & prepare data for plotting
indicator_data_1990_2015 = indicator_data[,c(2:3,4:4,35:60)]
colnames(indicator_data_1990_2015) <- sub("X", "", colnames(indicator_data_1990_2015))

our_indicator_data= melt(indicator_data_1990_2015, id=c("Indicator.Code","Indicator.Name","Country.Code"), value.name = "Year")

#setting up indicator data with country names from python @martijn
country_names = read.table("country_names.csv",sep = ",", header = TRUE)
country_wdi = country_names[,c(1:2,5:5)]

colnames(country_wdi) = c("Country.Code","Region","Region/Country Name")
our_indicator_data = merge(country_wdi,our_indicator_data,id = "Country.Code")
colnames(our_indicator_data) = c("Country Code","Region","Region/Country Name","Indicator Code","Indicator Name","Year","Value")

#Obtaining lists for information about regions & countries
all_region_and_country_codes=our_indicator_data[,1:3]
all_region_and_country_codes=unique(all_region_and_country_codes)
all_regions = subset(all_region_and_country_codes, all_region_and_country_codes$Region == "")
all_countries = subset(all_region_and_country_codes, all_region_and_country_codes$Region != "")

#did a work around because factor levels in R are stupid
general_regions = unique(all_countries['Region'])
region_list = c('Latin America & Caribbean','South Asia','Sub-Saharan Africa','Europe & Central Asia','Middle East & North Africa','East Asia & Pacific','North America')

#Subset to extract regions
regions_used = subset(all_regions, all_regions$'Region/Country Name' %in% region_list)
regions_used = regions_used[,-c(2)]

#FOR THE LINECHART ABOUT ABSOLUTE POVERTY
library(reshape)
library(reshape2)

world = c("WLD")
var_i = c("SI.POV.DDAY", "SP.POP.TOTL")
#subset the data with only the world and the two indicators above about population and people in poverty in %
graph_data = subset(our_indicator_data,our_indicator_data$'Indicator.Code' %in% var_i)
graph_data = subset(graph_data,graph_data$'Country.Code' %in% world)
#split the indicator column into two columns
wide = dcast(graph_data, Indicator.Name + Country.Code + variable ~ Indicator.Code,  value.var = "value" , fun.aggregate = mean, na.rm = T)
#this is a workaround for getting for each year the value for the two indicators into the same row
new1 <- wide[order(wide$SP.POP.TOTL),]
pop <- new1[1:26,]
new2 <- wide[order(wide$SI.POV.DDAY),]
si<-subset(new2,new2$SI.POV.DDAY != "NaN") 
si2 <-subset(si, select=c("SI.POV.DDAY", "variable"))
pop <- subset(pop, select = -SI.POV.DDAY )
wide = merge(pop,si2,id = "variable")
#omit all na values
wide <- na.omit(wide)
#divide the percentage indicator by 100 to get it scaled between 0 and 1
wide$SI.POV.DDAY <- wide$SI.POV.DDAY/100
#multiply the data to get the absolute number of people living in poverty
wide$absolute <- wide$SI.POV.DDAY * wide$SP.POP.TOTL



