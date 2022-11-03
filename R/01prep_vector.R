# prep vector
# produce sample points along road, 
# calculate distance to aldeias and distance to city (oiapoque)
# calculate buffers: 250, 500, 1000, 2000, 4000, 8000, 16000 
# intersect buffers with indigenous territory (Uaçá) and sustainable use PA (FLOTA)
# Studies show that 95% of accumulated deforestation in the Amazon 
# is located within a 5.5 km radius from roads and that 90% of annual fires occur 4 km from them. These roads are opened in the middle of the forest for logging, mining, and land grabbing.
# Within 32 km of highway and 5.5 km of all roads. I.e. fewer highways bu impact is far wider.

# all in EPSG 31976 and export as gpkg
library(tidyverse)
library(sf)
# for cropping
munis_norte <- read_sf("vector/vector_ibge/munis_norte.shp")
br156 <- read_sf("vector/br156_uaca.shp")
ti <- read_sf("vector/vector_funai/tis_poligonais_portarias", 
              options = "ENCODING=WINDOWS-1252",
              stringsAsFactors = FALSE) %>% 
  filter(terrai_cod=="47601") %>% st_transform(31976)
aldeias <- read_sf("vector/vector_funai/aldeias_uaca.shp", 
                   stringsAsFactors = FALSE) %>% st_transform(31976)
flota <- read_sf("vector/vector_mma/flota_ap.shp", 
                 stringsAsFactors = FALSE) %>% 
  st_crop(st_bbox(munis_norte))
ecoregions <- read_sf("vector/ecoregions_uaca.shp") %>% 
  st_transform(31976) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 1000) %>% 
  st_crop(st_bbox(munis_norte))
#180424.1 meters. 60 sections of approx 3 km
(st_length(st_union(br156))/1000)/60 
br156_points <- st_cast(br156, "LINESTRING") %>% st_union() %>%  
  st_sample(size = 60, type = "regular") %>% st_cast("POINT") %>% data.frame() %>% st_as_sf()
br156_points$aid <- rank(-st_coordinates(br156_points)[,2]) 

#manually ajusted
write_sf(br156_points, "br156_points.shp")
br156_points <- read_sf("vector/br156_points.shp")
mapview::mapview(br156_points, z="aid")
br156_points %>% 
  mutate(utm_x = st_coordinates(br156_points)[,1], 
         utm_y = st_coordinates(br156_points)[,2]) %>% 
  arrange(aid) %>%
  mutate(lag_x = lag(utm_x), 
         lag_y = lag(utm_y)) %>% 
  mutate(point_dist_m = sqrt((utm_x - lag_x)^2+(utm_y - lag_y)^2)) %>% 
  mutate(dist_oiap_km = cumsum(replace_na(point_dist_m,0))/1000) -> br156_points
mapview::mapview(br156_points, z="dist_oiap_km")  

ggplot() + 
  geom_sf(data = ecoregions, aes(fill=ECO_NAME)) +
  geom_sf(data=munis_norte, colour="blue", fill=NA) +
  geom_sf(data = ti, colour="yellow", fill=NA) + 
  geom_sf(data = flota, colour="green", fill=NA) +
  geom_sf(data = br156_points) + 
  scale_fill_grey()

#update midpoints with forestloss
outfile <- "C:/Users/user/Documents/CA/terra-indigena-estrada/vector/uaca_estrada.GPKG"
st_write(br156_points, dsn = outfile, 
         layer = "br156_points", delete_layer = TRUE, append = TRUE)
st_write(br156, dsn = outfile, 
         layer = "br156_line", delete_layer = TRUE, append = TRUE)
st_write(aldeias, dsn = outfile, 
         layer = "aldeias", delete_layer = TRUE, append = TRUE)
st_write(ecoregions, dsn = outfile, 
         layer = "ecoregions", delete_layer = TRUE, append = TRUE)
st_write(ti, dsn = outfile, 
         layer = "ti_uaca", delete_layer = TRUE, append = TRUE)
st_write(flota, dsn = outfile, 
         layer = "flota_ap", delete_layer = TRUE, append = TRUE)
st_write(munis_norte, dsn = outfile, 
         layer = "municipios_norte_ap", delete_layer = TRUE, append = TRUE)
st_layers(outfile)
