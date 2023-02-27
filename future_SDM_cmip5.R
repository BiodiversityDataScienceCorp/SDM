# future SDM 
# using CMIP5

# packages
install.packages("geodata")
install.packages("dismo")
install.packages("maptools")
install.packages("tidyverse")
install.packages("rJava")
install.packages("maps")

library(dismo)
library(maptools)
library(tidyverse)
library(rJava)
library(maps)
library(geodata)

# future SDM

# get climate data 
currentEnv <- clim # renaming from the last sdm because clim isn't very specific 

# to see the specifics 
futureEnv <- raster::getData(name = 'CMIP5', var = 'bio', res = 2.5,
                             rcp = 45, model = 'IP', year = 70) 
# in get data, you can specific with rcp, which is the greenhouse gas emission prediction,
# can also specific which model https://rdrr.io/cran/raster/man/getData.html
# and which year: 50 or 70 years from now

names(futureEnv) = names(currentEnv)
# look at current vs future climate vars
plot(currentEnv[[1]])
plot(futureEnv[[1]])

# predict  model onto future climate
geographicAreaFutureC5 <- crop(futureEnv, predictExtent)

# crop clim to the extent of the map you want
ranaPredictPlotFutureC5 <- raster::predict(ranaSDM, geographicAreaFutureC5) # predict the model to 

# for ggplot, we need the prediction to be a data frame 
raster.spdfFutureC5 <- as(ranaPredictPlotFutureC5, "SpatialPixelsDataFrame")
ranaPredictDfFutureC5 <- as.data.frame(raster.spdfFutureC5)

# plot in ggplot
wrld <- ggplot2::map_data("world")

# cmip 5
xmax <- max(ranaPredictDfFutureC5$x)
xmin <- min(ranaPredictDfFutureC5$x)
ymax <- max(ranaPredictDfFutureC5$y)
ymin <- min(ranaPredictDfFutureC5$y)


ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDfFutureC5, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  scale_size_area() +
  borders("state") +
  labs(title = "SDM of R. boylii Under CMIP 5 Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Env Suitability") +
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) 

