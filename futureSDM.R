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

bio10 <- cmip6_world("CNRM-CM6-1", "585", "2061-2080", var = "bioc", res=2.5, path="cmip6")
names(bio10) = names(clim)
names(bio10)=names(currentEnv)
# 2ill it work with fixed cmpi 6 data 
bio10 <- raster::stack(delta_raster_list)

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




### fixing temp data
# For these comparisons, because we used dismo::biovars(), all temperature 
# calculations are in degrees C, but the historic climate data is coming in 
# at 10 x degrees C (plot the bio1 layer for each to see the scales are an 
# order of magnitude different). 
plot(clim[[1]])
plot(bio10[[1]]) # includes Antarctica, so the scale goes even lower!

#To make meaningful deltas, multiply the 
# temperature layers for historic biovars by 0.1 before calculating delta
biovar_names <- names(bio10)
current_biovars <- clim
future_biovars <- bio10

temp_biovars <- c("bio1", "bio2", "bio4", "bio5", "bio6", "bio7", "bio8", 
                  "bio9", "bio10", "bio11")
# Create data frame to hold measures of variance
biovar_qc <- data.frame(name = biovar_names,
                        mean_delta = NA)
# Do calculation for each layer, seems quicker this way
delta_raster_list <- list()
for (biovar_name in biovar_names) {
  cat("Calcluating delta for ", biovar_name, "...\n", sep = "")
  multfac <- 1
  if (biovar_name %in% temp_biovars) {
    multfac <- 0.1
  }
  delta <- current_biovars[[biovar_name]] - (multfac * future_biovars[[biovar_name]])
  names(delta) <- biovar_name
  delta_raster_list[[biovar_name]] <- delta
  mean_delta <- raster::cellStats(x = delta, stat = "mean")
  biovar_qc$mean_delta[biovar_qc$name == biovar_name] <- mean_delta
}

# Do calculation for each layer, seems quicker this way
delta_raster_list <- list()
for (biovar_name in biovar_names) {
  cat("Calcluating delta for ", biovar_name, "...\n", sep = "")
  multfac <- 1
  if (biovar_name %in% temp_biovars) {
    multfac <- 0.1
  }
  delta <- multfac * future_biovars[[biovar_name]]
  names(delta) <- biovar_name
  delta_raster_list[[biovar_name]] <- delta
}







# 
standardizedCmip6 <- function(x) {
  for (biovar_name in biovar_names) {
    if (biovar_name %in% temp_biovars) {
      multfac <- 0.1
    } 
    else
      multfac <- 1
  }
}

# biovar_qc

# If we want to see where changes are happening, we can make a raster stack 
# and plot individual layers
# One a with "big" delta (bio4) just shows that it's driven by some Alaska and 
# Great Lakes; precip of wettest quarter (bio16) delta driven by BC and 
# Guatemala
# delta_stack <- raster::stack(x = delta_raster_list)
# plot(delta_stack[["bio4"]])
# plot(delta_stack[["bio16"]])


