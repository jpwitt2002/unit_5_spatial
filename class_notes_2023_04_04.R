# 2023-04-04
# JPW
# AIS

library(tidyverse)
library(sf)
library(mapdata)
library(lubridate)
library(marmap)

lat_bounds = c(25, 34)
lon_bounds = c(-82, -76)

ais_day = read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais_day)

USA_crit_hab = st_read("data/North_Atlantic_Right_Whale_Critical_Habitat/", 
                       "North_Atlantic_Right_Whale_Critical_Habitat")
world_map = map_data("worldHires", xlim=lon_bounds, ylim=lat_bounds)

ais_map_points = ggplot() + 
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group)) + 
  geom_sf(data=USA_crit_hab, fill="yellow", alpha=0.5) + 
  geom_point(data=ais_day, aes(x=LON, y=LAT, color=CallSign), size=0.5) + 
  coord_sf(1.3, xlim=lon_bounds, ylim=lat_bounds) +
  guides(color="none") +
  theme_classic()

# collapse data into a line for each boat 
# create crop of just critical habitat

ships_RW_intersect = ais_day %>%
  st_as_sf(coords=c("LON", "LAT"), crs=4269) %>%
  st_intersection(USA_crit_hab %>% dplyr::select(geometry))

# collapse data into a line for each boat 

law_breakers = ships_RW_intersect %>%
  filter(Length>20) %>% # unit in m, law for >65ft
  filter(SOG>10) # in knots
dim(law_breakers)  


length(unique(law_breakers$CallSign))  
# 26 law breakers at peak NARW cavling season

unique(law_breakers$VesselName) # hoes

illegal_paths = law_breakers %>%
  mutate(date_time = lubridate::ymd_hms(BaseDateTime)) %>%
  arrange(date_time) %>%
  group_by(CallSign) %>%
  summarize(do_union=F) %>% # created a multipoint
  st_cast("LINESTRING") %>%
  st_make_valid()

head(illegal_paths)  

law_breaking_map = ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_sf(data=USA_crit_hab, alpha = 0.5, color=NA, fill='yellow') +
  geom_sf(data=illegal_paths, aes(color=CallSign)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color="none") +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

illegal_paths_lengths = illegal_paths %>%
  mutate(track_length_m = st_length(geometry))

sum(illegal_paths_lengths$track_length_m)
  
