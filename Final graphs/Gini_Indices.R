library(reshape)
library(reshape2)
library(ineq)
library(plotly)

# indicator code for gini index
my_vars_c = c("SI.POV.GINI")
# subset the data - selected var 
indicator_datagini = subset(indicator_data,indicator_data$Indicator.Code %in% my_vars_c)

# LINE PLOT FOR THE COUNTRIES WITH THE HIGHEST GINI INDEXES
# extract the right years (25:60 is 1980 till 2015)
id = indicator_datagini[,c(1:3,4:4, 25:60)]
# remove the 'X' in the columns
colnames(id) <- sub("X", "", colnames(id))
# change the format
idm = melt(id, id=c("Indicator.Code","Indicator.Name","Country.Code", "Country.Name"), value.name = "Year")
# throw away the rows with missing data
idm <- na.omit(idm, cols="value")

# subset with only those countries
cl <- c("MDG","BDI","MWI","GNB","MOZ","ZMB","LSO")
idmc <- subset(idm,idm$Country.Code %in% cl)
# plot the gini idexes of those countries over time
x <- list( title = "Years" )
y <- list( title = "Gini Index" )
m <- list(l=150, r=20, b=70, t=10)
pline <- plot_ly(idmc, x = ~idmc$variable, y = ~idmc$value, color = ~idmc$Country.Code) %>%
  add_lines(line = list(shape = "spline"))  %>% layout(xaxis = x, yaxis = y, margin=m)
pline

# PLOT THE LORENZ CURVE OF ONE OF THE COUNTRIES (if a flat line occurs, there is no data)
# Get the indicators for the lorenz curve
my_var = c( "SI.DST.05TH.20","SI.DST.02ND.20","SI.DST.03RD.20", "SI.DST.04TH.20", "SI.DST.FRST.20")
# fill in country code in V1 and run code below
V1 = c("LSO")
# subset the data with only the country and the needed indicators
country_data = subset(indicator_data,indicator_data$Country.Code %in% V1)
COUNTRY_LC_V1 = subset(country_data,country_data$Indicator.Code %in% my_var)
# slice the needed data for the desirable year
COUNTRY_2015 = COUNTRY_LC_V1[,c(1:4,55:55)]
# create a vector for the lorenz curve function
p20 = c( 20, 40, 60 , 80, 100)
# sort the values of the indicators. Make sure the year is the right one
d20 = sort(COUNTRY_2015$X2010)
# plot the lorenz curve
Lc.c <- Lc(d20, p20)
plot(Lc.c)

gini_index_country = Gini(d20)










# get the countries with the 10 highest gini indexes. Some countries will be double, but that is not a problem
idm2 <- idm[order(idm$value,decreasing=T)[1:10],]
# make a vector of the countries above
v <- idm2[,3]
