library(reshape)
library(reshape2)
library(ineq)
library(plotly)

# Read the data in correctly
mydata = read.table("WDIData.csv",sep=",",fileEncoding="UTF-8-BOM",header=TRUE)  

# indicator code for gini index
my_vars_c = c("SI.POV.GINI")
# subset the data - selected var 
indicator_data = subset(mydata,mydata$Indicator.Code %in% my_vars_c)

# LINE PLOT FOR THE COUNTRIES WITH THE HIGHEST GINI INDEXES
# extract the right years (25:60 is 1980 till 2015)
id = indicator_data[,c(1:3,4:4, 25:60)]
# remove the 'X' in the columns
colnames(id) <- sub("X", "", colnames(id))
# change the format
idm = melt(id, id=c("Indicator.Code","Indicator.Name","Country.Code", "Country.Name"), value.name = "Year")
# throw away the rows with missing data
idm <- na.omit(idm, cols="value")
# get the countries with the 10 highest gini indexes. Some countries will be double, but that is not a problem
idm2 <- idm[order(idm$value,decreasing=T)[1:10],]
# make a vector of the countries above
v <- idm2[,3]
# subset with only those countries
idm3 <- subset(idm,idm$Country.Code %in% v)
# plot the gini idexes of those countries over time
x <- list( title = "Years" )
y <- list( title = "Gini Index" )
m <- list(l=150, r=20, b=70, t=10)
pline <- plot_ly(idm3, x = ~idm3$variable, y = ~idm3$value, color = ~idm3$Country.Code) %>%
  add_lines(line = list(shape = "spline"))  %>% layout(xaxis = x, yaxis = y, margin=m)
pline

# PLOT THE LORENZ CURVE OF ONE OF THE COUNTRIES (if a flat line occurs, there is no data)
# Get the indicators for the lorenz curve
my_var = c( "SI.DST.05TH.20","SI.DST.02ND.20","SI.DST.03RD.20", "SI.DST.04TH.20", "SI.DST.FRST.20")
# fill in country code in V1 and run code below
V1 = c("BOL")
# subset the data with only the country and the needed indicators
country_data = subset(mydata,mydata$Country.Code %in% V1)
COUNTRY_LC_V1 = subset(country_data,country_data$Indicator.Code %in% my_var)
# slice the needed data for the desirable year
COUNTRY_2015 = COUNTRY_LC_V1[,c(1:4,60:60)]
# create a vector for the lorenz curve function
p20 = c( 20, 40, 60 , 80, 100)
# dort the values of the indicators
d20 = sort(COUNTRY_2015$X2015)
# plot the lorenz curve
Lc.ARG <- Lc(d20, p20)
plot(Lc.ARG)
gini_index_country_2015 = Gini(d20)





