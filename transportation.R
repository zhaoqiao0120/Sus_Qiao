install.packages("sf")
install.packages("raster")
install.packages("spData")
install.packages("dplyr")
install.packages("stplanr")
install.packages("tmap")
devtools::install_github("Nowosad/spDataLarge")

library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(dplyr)
library(stringr) # for working with strings (pattern matching)
library(spDataLarge)   # load larger geographic data
library(stplanr)      # geographic transport data package
library(tmap)         # visualization package (see Chapter 8)

vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

############12.3#####
data(bristol_zones)
names(bristol_zones)
data(bristol_od) #travel data
names(bristol_od)

zones_attr = bristol_od %>% 
  group_by(o) %>% 
  summarize_if(is.numeric, sum) %>% #the total number of people living in each zone
  dplyr::rename(geo_code = o)

zones_joined = left_join(bristol_zones, zones_attr, by = "geo_code")

zones_od = bristol_od %>% 
  group_by(d) %>% 
  summarize_if(is.numeric, sum) %>% 
  dplyr::select(geo_code = d, all_dest = all) %>% 
  inner_join(zones_joined, ., by = "geo_code")

qtm(zones_od, c("all", "all_dest")) +
  tm_layout(panel.labels = c("Origin", "Destination"))

############12.4#####
od_top5 = bristol_od %>% 
  arrange(desc(all)) %>% 
  top_n(5, wt = all)

od_intra = filter(bristol_od, o == d)
od_inter = filter(bristol_od, o != d)
desire_lines = od2line(od_inter, zones_od)
#> Creating centroids representing desire line start and end points.
qtm(desire_lines, lines.lwd = "all")

############12.5#####
desire_lines$distance = as.numeric(st_length(desire_lines))
desire_carshort = dplyr::filter(desire_lines, car_driver > 300 & distance < 5000)

route_carshort = line2route(desire_carshort, route_fun = route_osrm)
desire_carshort$geom_car = st_geometry(route_carshort)

############12.6#####
desire_rail = top_n(desire_lines, n = 3, wt = train)

ncol(desire_rail)
#> [1] 10
desire_rail = line_via(desire_rail, bristol_stations)
ncol(desire_rail)
#> [1] 13

############12.7#####
summary(bristol_ways)

ways_freeway = bristol_ways %>% filter(maxspeed == "70 mph") 
ways_sln = SpatialLinesNetwork(ways_freeway)#represent route networks simultaneously as graphs and a set of geographic lines
slotNames(ways_sln)
#> [1] "sl"          "g"           "nb"          "weightfield"
weightfield(ways_sln)
#> [1] "length"
class(ways_sln@g)

e = igraph::edge_betweenness(ways_sln@g)#the number of shortest paths passing through each edge
plot(ways_sln@sl$geometry, lwd = e / 500)

############12.8#####
route_rail = desire_rail %>% 
  st_set_geometry("leg_orig") %>% 
  line2route(route_fun = route_osrm) %>% 
  st_set_crs(4326)

route_cycleway = rbind(route_rail, route_carshort)
route_cycleway$all = c(desire_rail$all, desire_carshort$all)

qtm(route_cycleway, lines.lwd = "all")
