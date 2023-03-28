# 2023-03-28
# JPW

# rasters

library(tidyverse)
library(raster)
library(mapdata)
library(marmap) # getNOAA.bathy()

chl_raster = raster("data/AQUA_MODIS.20020701_20220731.L3m.MC.CHL.chlor_a.9km.nc")

names(chl_raster) = "chl_a"
names(chl_raster)

# change to data frame
chl_pts = rasterToPoints(chl_raster, spatial=T)
head(chl_pts)
chl_df = data.frame(chl_pts)
head(chl_df)

hist(chl_df$chl_a)
max(chl_df$chl_a)
# log transform to see variation
hist(log10(chl_df$chl_a))

# colors?

cols = rainbow(7, rev=T)[-1]

global_chl_map = ggplot() + 
  geom_raster(data = chl_df, aes(x=x, y=y, fill=log10(chl_a))) +
  scale_fill_gradientn(colors=cols, limits=c(-1.5, 0.75), name = "log_10(chl_a)") + 
  theme_classic()
ggsave(global_chl_map, filename = "figures/global_chl_July2002-July2022.pdf", 
       device="pdf", height=5, width=9)



# Gulf of Maine
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

# crop
chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))

chl_GOM_df = data.frame( rasterToPoints(chl_GOM_raster, spatial=T) )
head(chl_GOM_df)

world_map = map_data("worldHires")

GOM_chl_map = ggplot() + 
  geom_raster(data=chl_GOM_df, aes(x=x, y=y, fill=log10(chl_a))) + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill="darkgrey") + 
  scale_fill_gradientn(colors=cols, limits=c(-1, 1.75), ) + 
  theme_bw() + 
  coord_fixed(1.3, xlim=lon_bounds, ylim=lat_bounds, expand=F)
  

# bathemetry 

# Gulf of Maine
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], 
                      lon2 = lon_bounds[2], 
                      lat1 = lat_bounds[1], 
                      lat2 = lat_bounds[2], 
                      resolution = 4) # in arcminutes (whatever that means)
class(bath_m_raw)  
  

