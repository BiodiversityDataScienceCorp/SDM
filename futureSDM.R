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

# get climate data (two ways, either cmip5 or cmip6)
futureEnv <- raster::getData(name = 'CMIP5', var = 'bio', res = 2.5,
                rcp = 45, model = 'IP', year = 70)

names(futureEnv)=names(currentEnv)

bio10<- cmip6_world("CNRM-CM6-1", "585", "2061-2080", var = "bioc", res=2.5, path="cmip6")

names(bio10)=names(currentEnv)

# plot(bio10) # to see what cmip6 data looks like

# predict  model onto future climate (choose with climate data here)
geographicAreaFutureC5 <- crop(futureEnv, predictExtent)
geographicAreaFutureC6 <- crop(bio10, predictExtent)

# crop clim to the extent of the map you want
ranaPredictPlotFutureC5 <- raster::predict(ranaSDM, geographicAreaFutureC5) # predict the model to 
ranaPredictPlotFutureC6 <- raster::predict(ranaSDM, geographicAreaFutureC6) # predict the model to 


# for ggplot, we need the prediction to be a data frame 
raster.spdfFutureC5 <- as(ranaPredictPlotFutureC5, "SpatialPixelsDataFrame")
ranaPredictDfFutureC5 <- as.data.frame(raster.spdfFutureC5)

ranaPredictDfFutureC6 <- as.data.frame(ranaPredictPlotFutureC6, xy=TRUE)

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

# cmip 6
xmax <- max(ranaPredictDfFutureC6$x)
xmin <- min(ranaPredictDfFutureC6$x)
ymax <- max(ranaPredictDfFutureC6$y)
ymin <- min(ranaPredictDfFutureC6$y)


ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDfFutureC6, aes(x = x, y = y, fill = maxent)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  scale_size_area() +
  borders("state") +
  labs(title = "SDM of R. boylii Under CMIP6 Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Env Suitability") +
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) 




