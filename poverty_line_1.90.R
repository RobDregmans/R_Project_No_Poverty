

mydata_utf8 <- read.table("WDI_csv/WDIData.csv",header = TRUE, sep=",")
#####only the $1.90 line
poverty_gap_data <- subset(mydata_utf8,mydata_utf8$Indicator.Code=="SI.POV.GAPS")
poverty_gap_data_1990 <- poverty_gap_data[,c(1:3,4:4, 35:60)]
# remove the 'X' in the columns
colnames(poverty_gap_data_1990) <- sub("X", "", colnames(poverty_gap_data_1990))
#remove the rows without any data
poverty_gap_withoutNA <- poverty_gap_data_1990[-which(rowSums(is.na(poverty_gap_data_1990))>24),]
#reshape the data for drawing graphs
poverty_melt = melt(poverty_gap_withoutNA, id=c("Indicator.Code","Indicator.Name","Country.Code", "Country.Name"), value.name = "Year")
#stacked area graph
######doesn't work######
p <- ggplot(poverty_melt, aes(x = variable, y = value, fill = Country.Code)) +  geom_area()  #面积图
#set the y axis scale
p + ylim(0,max(poverty_melt$value))
p + scale_y_continuous(limits=c(0, 100), breaks=NULL)

##poverty line $1.90, six regions in one graph
regions_melt <- subset(poverty_melt, Country.Name =="East Asia & Pacific" | Country.Name =="Europe & Central Asia"| Country.Name =="Latin America & Caribbean"| Country.Name =="Middle East & North Africa"| Country.Name =="South Asia"| Country.Name =="Sub-Saharan Africa")
gg <- ggplot(regions_melt, aes(x=as.numeric(as.character(variable)), y=value))
gg <- gg + geom_area(aes(colour=Country.Name, fill=Country.Name))
gg

##three poverty lines of one single region in one graph
#needs another kind of data type

poverty_1.90_per_region = subset(poverty_1.90,poverty_1.90$`Region/Country Name`%in% region_list)
poverty_1.90_per_region = poverty_1.90_per_region[,-c(2)]