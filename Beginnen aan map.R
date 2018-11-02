library(rgeos)
library(maptools)
library(gpclib)  # may be needed, may not be

# MAP
np_dist <- readShapeSpatial("Africa_SHP/Africa.shp")
# VERIFY IT LOADED PROPERLY
plot(np_dist)

library(ggplot2)
np_dist <- fortify(np_dist, region = "NAME_3")
np_dist$id <- toupper(np_dist$id)  #change ids to uppercase
ggplot() + geom_map(data = edu63, aes(map_id = District, fill = PASS.PERCENT), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)