library(reshape)
library(reshape2)

# Read the data in correctly
mydata = read.table("WDIData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE)  
#mydat_sdg = read.table("data/sdg/SDGData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE) 

# drop off the final blank column
#mydata = mydata_utf8[,1:62]

#indicator code for gini index
my_vars_c = c("SI.POV.GINI")
# subset the data - selected var 
indicator_data = subset(mydata,mydata$Indicator.Code %in% my_vars_c)

#LINE PLOT FOR THE COUNTRIES WITH THE HIGHEST GINI INDEXES
#extract the right years (25:60 is 1980 till 2015)
id = indicator_data[,c(1:3,4:4, 25:60)]
#remove the 'X' in the columns
colnames(id) <- sub("X", "", colnames(id))
#change the format
idm = melt(id, id=c("Indicator.Code","Indicator.Name","Country.Code", "Country.Name"), value.name = "Year")
#throw away the rows with missing data
idm <- na.omit(idm, cols="value")
#get the countries with the 10 highest gini indexes. Some countries will be double, but that is not a problem
idm2 <- idm[order(idm$value,decreasing=T)[1:10],]
#make a vector of the countries above
v <- idm2[,3]
#subset with only those countries
idm3 <- subset(idm,idm$Country.Code %in% v)
#plot the gini idexes of those countries over time
pline <- plot_ly(idm3, x = ~idm3$variable, y = ~idm3$value, color = ~idm3$Country.Code) %>%
  add_lines(line = list(shape = "spline"))
pline