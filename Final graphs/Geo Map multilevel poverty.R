#Selecting MDI Data, drop last 3 columns with no values
MPIData = read.table("MPIData.csv",sep=";",header=TRUE)  
colnames(MPIData)[1] <- "CountryCode"
MPIData = MPIData[,1:20]

#Selecting MDI Data, drop last 3 columns with no values
MPIData2016 = read.table("MPIData2016.csv",sep=";",header=TRUE)  
colnames(MPIData2016)[1] <- "PublishYear"


# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

MPIData2016$hover <- with(MPIData2016, paste
                      ( "Year:", SurveyYear,
                            "<br> Country:", Country,
                            "<br> Share of the pop. in Multi Level Poverty: ", Headcount.ratio ,"%",
                            "<br> Multi Poverty Index:", MPI,
                            "<br> Child school attendace:", ChildSchoolAttendance))
                          

# specify map projection/options
g <- list(
  resolution = 5,
  showcoastlines = T,
  countrycolor = toRGB("grey"),
  coastlinecolor = toRGB("grey"),
  projection = list(type = 'Mercator') 
)

k <- plot_geo(MPIData2016) %>%
  add_trace(z = ~Headcount.ratio,text = ~hover, color = ~Headcount.ratio, colors = 'YlGnBu' ,
            locations = ~CountryCode, 
            marker = list(line = l,color = 'rgb(255,255,255)')) %>%
  
  colorbar(title = '/nShare of population living in MPI') %>%
  layout(
    title = 'Share of population per country trapped in multi level poverty',
    geo = g )
k