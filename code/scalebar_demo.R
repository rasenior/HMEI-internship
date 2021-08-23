
# Load packages/functions
source("code/map_scalebar.R")
library(ggspatial)

# Load some example data
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Option 1 (probably easiest)
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

# Option 2 (an old method I once used before the above option existed)
ggplot(data = world) +
  geom_sf() +
  geom_segment(aes(x=-102,xend=-102,y=8.5,yend=10),
               size=0.5,arrow = arrow(length = unit(0.15,"cm"),
                                      ends = "last",type = "closed"))+
  geom_text(aes(x=-102,y=10.7,label="N"),size=3) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97)) +
  scale_bar(lon = -102, lat = 7, 
              distance_lon = 1000, distance_lat = 50, distance_legend = 100, 
              dist_unit = "km", orientation = FALSE)
