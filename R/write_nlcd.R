library(terra)
library(landscapetools)
library(landscapemetrics)
library(raster)
library(dplyr)
library(motif)
library(tmap)
library(sf)
library(purrr)
library(ggplot2)

# 1. oba rastry podzielić na osobne rastry dla każdej kategorii
# 2. pokroić rastry w kafelki 100x100 komórek
# 3. złączyć rastry w dwie listy dla 2011 i 2021 roku
# 4. dla każdego kafelka policzyć ent, mutinf, relmutinf
# 5. zrobić plot ent~relmutinf dla wszystkich komórek w podziale na 
# 6. dla każdej pary kafelków policzyć miary odległości


# dodać grupowanie wg udziału danej kategorii w kafelku
# shei landscape metrics


# read input rasters ------------------------------------------------------
nlcd_2011 = rast("geodata/NLCD_2011_Land_Cover_L48_20210604_8gGKoJRuDHcmwlsvc7CC.tiff")
nlcd_2021 = rast("geodata/NLCD_2021_Land_Cover_L48_20230630_8gGKoJRuDHcmwlsvc7CC.tiff")


# crop rasters to nicer dimensions ----------------------------------------
cellsize = rev((floor(dim(nlcd_2011)[1:2] / 100) * 100) * 30) #wtf did i do

my_grid_geom = st_make_grid(st_as_sfc(st_bbox(nlcd_2011)), cellsize = cellsize)
my_grid = st_sf(geom = my_grid_geom[1])

plot(nlcd_2011)
plot(my_grid, add = TRUE)

nlcd_2011_cropped = crop(nlcd_2011, my_grid)
nlcd_2021_cropped = crop(nlcd_2021, my_grid)
dim(nlcd_2011_cropped) #nice dimensions that won't produce rasters with NA values


# fix category values -----------------------------------------------------
input_categories = nlcd_2021_cropped %>% 
  freq() %>% 
  as.data.frame() %>% 
  select(value) %>% 
  unlist() %>% 
  unname()

output_categories = 1:length(input_categories)

rcl_matrix = cbind(input_categories, output_categories)

nlcd_2011_reclas = classify(nlcd_2011_cropped, rcl_matrix)
nlcd_2021_reclas = classify(nlcd_2021_cropped, rcl_matrix)

remove(rcl_matrix, cellsize, input_categories, nlcd_2011, nlcd_2011_cropped, nlcd_2021, nlcd_2021_cropped, my_grid, my_grid_geom)


# prepare tiles for later raster tile extraction --------------------------
tilesize = 100 * 30 #tiles that are 100 by 100 cells where each cell is 30x30m

my_grid_geom = st_make_grid(st_as_sfc(st_bbox(nlcd_2011_reclas)), cellsize = tilesize)
my_grid = st_sf(geom = my_grid_geom)

plot(nlcd_2011_reclas)
plot(my_grid[1:1000,], add = TRUE)


# split rasters into 100x100 cells binary tiles ---------------------------
output_tiles = data.frame(category = NA, plot_id = NA, y2011 = NA, y2021 = NA)[0,]

binarize = function(data, id){
  output = data %>% 
    classify(rcl = cbind(id, 1), others = 0)
  return(output)
}

set.seed(123)
for (i in output_categories){
  map1 = binarize(nlcd_2011_reclas, id = i) #binarize maps
  map2 = binarize(nlcd_2021_reclas, id = i)
  
  tile_sample = my_grid[sample(nrow(my_grid), 25), ] #sample 20 tiles for later extraction
  
  tiles_2011 = sample_lsm(map1, tile_sample,
                           level = "landscape", metric = c("ent"),
                           return_raster = TRUE) %>% 
    select(plot_id, raster_sample_plots)
  
  tiles_2021 = sample_lsm(map2, tile_sample,
                          level = "landscape", metric = c("ent"),
                          return_raster = TRUE) %>% 
    select(plot_id, raster_sample_plots)
  
  output = merge(tiles_2011, tiles_2021, by = "plot_id") %>% 
    rename(y2011 = raster_sample_plots.x, y2021 = raster_sample_plots.y) %>% 
    mutate(category = i)
  
  output_tiles = rbind(output_tiles, output)
}


# save output files -------------------------------------------------------
# y2011 = do.call(c, output_tiles$y2011)
# y2011 = stack(y2011)
# writeRaster(y2011, filename = "geodata/y2011_tiles.tiff", overwrite = TRUE)


# calculate landscape metrics ---------------------------------------------
output_tiles$ent2011 = lsm_l_ent(output_tiles$y2011)$value
output_tiles$ent2021 = lsm_l_ent(output_tiles$y2021)$value

output_tiles$mutinf2011 = lsm_l_mutinf(output_tiles$y2011)$value
output_tiles$mutinf2021 = lsm_l_mutinf(output_tiles$y2021)$value

output_tiles$relmutinf2011 = lsm_l_relmutinf(output_tiles$y2011)$value
output_tiles$relmutinf2021 = lsm_l_relmutinf(output_tiles$y2021)$value

output_tiles$diffent = abs(output_tiles$ent2011 - output_tiles$ent2021)
output_tiles$diffrelmutinf = abs(output_tiles$relmutinf2011 - output_tiles$relmutinf2021)


output_tiles$category = as.factor(output_tiles$category)

output_tiles %>% 
  ggplot(aes(ent2011, relmutinf2011, col = category, fill = category)) +
  geom_point() +
  coord_fixed(ratio = 1)





# sample 50 pairs of rasters ----------------------------------------------
library(cluster)
# ?pam

klas = pam(output_tiles[c("diffent", "diffrelmutinf")], k = 50)
klas
output_tiles$clustering = klas$clustering

output_tiles %>% 
  ggplot(aes(diffent, diffrelmutinf, color = as.factor(clustering))) +
  geom_point() +
  coord_fixed(ratio = 1)

# df2[klas$id.med, ]
# df2
