install.packages("geodata")
library("geodata")

# future SDM

# get climate data
futureEnv <- raster::getData(name = 'CMIP5', var = 'bio', res = 10,
                rcp = 45, model = 'IP', year = 70)

names(futureEnv)=names(currentEnv)

bio10 <- cmip6_world("CNRM-CM6-1", "585", "2061-2080", res=10, path="cmip6")

plot(bio10)

# predict  model onto future climate 
geographicAreaFuture <- crop(bio10, predictExtent)

# crop clim to the extent of the map you want
ranaPredictPlotFuture <- raster::predict(ranaSDM, geographicAreaFuture) # predict the model to 

# for ggplot, we need the prediction to be a data frame 
raster.spdfFuture <- as(ranaPredictPlotFuture, "SpatialPixelsDataFrame")
ranaPredictDfFuture <- as.data.frame(raster.spdfFuture)

# plot in ggplot
wrld <- ggplot2::map_data("world")
xmax <- max(ranaPredictDfFuture$x)
xmin <- min(ranaPredictDfFuture$x)
ymax <- max(ranaPredictDfFuture$y)
ymin <- min(ranaPredictDfFuture$y)


ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDfFuture, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  scale_size_area() +
  borders("state") +
  labs(title = "SDM of R. boylii Under Current Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Probability of Presence") +
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) 



