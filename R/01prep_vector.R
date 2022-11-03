# prep vector
# produce sample points along road, 
# calculate distance to city (oiapoque)
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
# Used to split buffer polygons
br156long <- read_sf("vector/br156_uaca_long.shp") %>% 
  st_make_valid()
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

# Manually ajusted in QGIS
#write_sf(br156_points, "br156_points.shp")
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

# Check
ggplot() + 
  geom_sf(data = ecoregions, aes(fill=ECO_NAME)) +
  geom_sf(data=munis_norte, colour="blue", fill=NA) +
  geom_sf(data = ti, colour="yellow", fill=NA) + 
  geom_sf(data = flota, colour="green", fill=NA) +
  geom_sf(data = br156_points) + 
  scale_fill_grey()

# distance to TI and FLOTA
br156_points$dist_uaca_m <- round(as.numeric(st_distance(br156_points, 
                                                         st_cast(ti, "MULTILINESTRING")),1))
br156_points$dist_flota_m <- round(as.numeric(st_distance(br156_points, 
                                                          st_cast(flota, "MULTILINESTRING")),1))
# Join with layers
br156_points %>%
mutate(point_dist_m = round(point_dist_m,1),
         dist_oiap_km = round(dist_oiap_km,3)) %>% 
  left_join(
st_intersection(br156_points, ti) %>% data.frame() %>% 
  mutate(flag_uaca = 1) %>% 
  select(aid, flag_uaca)) %>% left_join(
st_intersection(br156_points, flota) %>% data.frame() %>% 
  mutate(flag_flota = 1) %>% 
  select(aid, flag_flota)
) %>% left_join( 
  st_intersection(br156_points, ecoregions) %>% data.frame() %>% 
    select(aid, ECO_NAME) ) %>%
  select(aid, point_dist_m, dist_oiap_km, flag_uaca, dist_uaca_m, 
         flag_flota, dist_flota_m, 
         ECO_NAME) %>% 
  mutate(flag_uaca = replace_na(flag_uaca,0), 
         flag_flota = replace_na(flag_flota,0), 
         dist_uaca_m = round(if_else(flag_uaca==0,-dist_uaca_m, 
                               dist_uaca_m),1), 
         dist_flota_m = round(if_else(flag_flota==0,-dist_flota_m, 
                               dist_flota_m),1)) -> br156_points_out
br156_points_out %>% data.frame()

# Check
mapview::mapview(flota, label="FLOTA", col.regions ="magenta") +
mapview::mapview(ti, z="terrai_nom") +
mapview::mapview(br156_points_out, z="dist_uaca_m") 

# Buffers
br156_points_out_b250m <- st_buffer(br156_points_out, dist=250) %>% 
  mutate(buff_dist = 250)
br156_points_out_b500m <- st_buffer(br156_points_out, dist=500) %>% 
  mutate(buff_dist = 500)
br156_points_out_b1km <- st_buffer(br156_points_out, dist=1000) %>% 
  mutate(buff_dist = 1000)
br156_points_out_b2km <- st_buffer(br156_points_out, dist=2000) %>% 
  mutate(buff_dist = 2000)
br156_points_out_b4km <- st_buffer(br156_points_out, dist=4000) %>% 
  mutate(buff_dist = 4000)
br156_points_out_b8km <- st_buffer(br156_points_out, dist=8000) %>% 
  mutate(buff_dist = 8000)
br156_points_out_b16km <- st_buffer(br156_points_out, dist=16000) %>% 
  mutate(buff_dist = 16000)
bind_rows(br156_points_out_b250m, 
          br156_points_out_b500m, 
          br156_points_out_b1km, 
          br156_points_out_b2km, 
          br156_points_out_b4km, 
          br156_points_out_b8km, 
          br156_points_out_b16km) %>% 
  mutate(buff_id = paste(aid, buff_dist, sep="_")) -> br156_points_buffers
br156_points_buffers$buff_area_km2 <- round(as.numeric(units::set_units(st_area(br156_points_buffers),km^2)), 3)
# Select only relevant to buffer
br156_points_buffers %>% 
  select(aid, buff_id, buff_dist, buff_area_km2) -> br156_points_buffers
# % coverage of FLOTA, Uaçá in each buffer.

# Split buffer polygons by road
bsplit_250 <- lwgeom::st_split(br156_points_out_b250m, br156) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(aid = factor(aid))
bsplit_500 <- lwgeom::st_split(br156_points_out_b500m, br156) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(aid = factor(aid))
bsplit_1km <- lwgeom::st_split(br156_points_out_b1km, br156long) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(aid = factor(aid))
bsplit_2km <- lwgeom::st_split(br156_points_out_b2km, br156long) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(aid = factor(aid))
bsplit_4km <- lwgeom::st_split(br156_points_out_b4km, br156long) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(aid = factor(aid))
bsplit_8km <- lwgeom::st_split(br156_points_out_b8km, br156long) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(aid = factor(aid))
bsplit_16km <- lwgeom::st_split(br156_points_out_b16km, br156long) %>% 
  st_collection_extract("POLYGON") %>% 
  mutate(aid = factor(aid))
# check
mapview::mapview(flota, label="FLOTA", col.regions ="magenta") +
  mapview::mapview(ti, z="terrai_nom") +
  mapview::mapview(bsplit_16km, z="aid") 
# Join
bind_rows(bsplit_250, 
          bsplit_500, 
          bsplit_1km, 
          bsplit_2km, 
          bsplit_4km, 
          bsplit_8km, 
          bsplit_16km) %>% 
  mutate(buff_id = paste(aid, buff_dist, sep="_")) %>% 
  arrange(aid, buff_dist) -> br156_split_buffers
br156_split_buffers$buff_area_km2 <- round(as.numeric(units::set_units(st_area(br156_split_buffers),km^2)), 3)
# Select only relevant to buffer
br156_split_buffers %>% 
  arrange(aid, buff_dist) %>%
  select(aid, buff_id, buff_dist, buff_area_km2, dist_oiap_km) -> br156_split_buffers
# Identify east west
br156_split_buffers$cent_x <- st_coordinates(st_centroid(br156_split_buffers))[,1]
br156_split_buffers %>% 
  group_by(buff_id) %>% 
  mutate(cent_x_min = min(cent_x)) %>% 
  ungroup() %>% 
  mutate(lado_estrada = if_else(cent_x_min==cent_x,"oeste", "leste")) %>% 
  mutate(buff_lado_id = paste(buff_id, lado_estrada, sep="_")) -> br156_split_buffers
# check
mapview::mapview(flota, label="FLOTA", col.regions ="magenta") +
  mapview::mapview(ti, z="terrai_nom") +
  mapview::mapview(br156_split_buffers, z="lado_estrada") 

# % coverage of FLOTA, Uaçá in each buffer.
br156_split_buffers %>% data.frame()
st_intersection(br156_split_buffers, ti) %>% 
  select(buff_lado_id, terrai_nom) -> br156_split_buffers_uaca
br156_split_buffers_uaca$uaca_area_km2 <- round(as.numeric(units::set_units(st_area(br156_split_buffers_uaca),km^2)), 3)
br156_split_buffers_uaca %>% data.frame()
# FLOTA
st_intersection(br156_split_buffers, flota) %>% 
  select(buff_lado_id, NOME_UC1) -> br156_split_buffers_flota
br156_split_buffers_flota$flota_area_km2 <- round(as.numeric(units::set_units(st_area(br156_split_buffers_flota),km^2)), 3)
br156_split_buffers_flota %>% data.frame()
# Calculate percentage of each buffer
br156_split_buffers %>% 
  left_join(data.frame(br156_split_buffers_uaca) %>% 
              select(buff_lado_id, uaca_area_km2)) %>% 
  left_join(data.frame(br156_split_buffers_flota) %>% 
              select(buff_lado_id, flota_area_km2)) %>% 
  mutate(uaca_area_km2 = replace_na(uaca_area_km2, 0), 
         flota_area_km2 = replace_na(flota_area_km2, 0)) %>% 
  arrange(aid, buff_dist) %>% 
  mutate(sem_prot_km2 = buff_area_km2 - (uaca_area_km2 + flota_area_km2)) %>% 
  mutate(uaca_per = round((uaca_area_km2/buff_area_km2) * 100, 1), 
         flota_per = round((flota_area_km2/buff_area_km2) * 100, 1), 
         sem_per = round((sem_prot_km2/buff_area_km2) * 100, 1)) %>% 
  mutate(tot_per = uaca_per + flota_per + sem_per) %>%
  select(aid, buff_dist, buff_lado_id, lado_estrada, dist_oiap_km, 
         buff_area_km2, uaca_area_km2, flota_area_km2, sem_prot_km2, 
         uaca_per, flota_per, sem_per, tot_per) -> br156_split_buffers_out


# Export as gpkg
outfile <- "C:/Users/user/Documents/CA/terra-indigena-estrada/vector/uaca_estrada.GPKG"
st_write(br156_points_out, dsn = outfile, 
         layer = "br156_points", delete_layer = TRUE, append = TRUE)
st_write(br156_points_buffers, dsn = outfile, 
         layer = "br156_points_buffers", delete_layer = TRUE, append = TRUE)
st_write(br156_split_buffers_out, dsn = outfile, 
         layer = "br156_split_buffers", delete_layer = TRUE, append = TRUE)
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
