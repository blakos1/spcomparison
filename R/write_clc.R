library(raster)
library(motif)
library(stars)
library(dplyr)
library(purrr)
library(terra)
library(ggplot2)
library(tmap)
library(landscapemetrics)

# 1- antro
# 2 - ter rolne
# 3 - lasy
# 4 - tereny krzewiaste
# 5 - tereny z rzadka roslinnoscia
# 6 - obszary podmokle
# 7 - obszary wodne


# read and reclassify input rasters ---------------------------------------
clc1990_2 = terra::rast("data/clc1990_2.tif")
clc2018_2 = terra::rast("data/clc2018_2.tif")

input = c(1,2,3,4,5,6,7)
# output = c(1,1,2,2,1,2,1)
output = c(1,1,2,2,1,2,1)
matrix.rcl = cbind(input,output)

clc1990_2 = terra::classify(clc1990_2, matrix.rcl)
clc2018_2 = terra::classify(clc2018_2, matrix.rcl)

plot(clc2018_2)
freq(clc2018_2)


# exclude tiles that are outside the raster area --------------------------
window_size = 100 #set dimensions of output tiles (window = 100 outputs a 100x100 cell tile)

compare = lsp_compare(clc1990_2, clc2018_2,
                      type = "cove",
                      dist_fun = "jensen-shannon", 
                      window = window_size)

my_breaks = c(0, 0.001, 0.01, 0.1, 1.01)
tm_shape(compare) +
  tm_raster("dist", breaks = my_breaks, palette = "-viridis") +
  tm_layout(legend.outside = TRUE)

ids_to_keep = compare %>% 
  as.data.frame() %>% 
  na.omit() %>% 
  select(id) %>% 
  unlist() %>% 
  unname()

clc1990_2_lsp = lsp_add_sf(clc1990_2, window = window_size) %>% 
  subset(id %in% ids_to_keep)

clc2018_2_lsp = lsp_add_sf(clc2018_2, window = window_size) %>% 
  subset(id %in% ids_to_keep)

identical(clc1990_2_lsp$id, clc2018_2_lsp$id) #checks if both rasters contain the exact same list of tiles


# cut input rasters into tiles --------------------------------------------
# this functions takes clc file as input,
# slices it into 100x100 cells tiles,
# converts them into matrixes to remove their spatial extents
# and then merges all of the tiles together into a list that can be read as a rasterstack

lsp_extract2 = function(id){
  sim1 = lsp_extract(clc2018_2, window = window_size, id = id)

  na_test = sum(is.na(sim1[]))
  if (any(dim(sim1)[1:2] != window_size)){ #if tile dimensions are not 100x100 -> NA
    NA
  } else if ((na_test / (ncell(sim1))) > 0.9){ #if the percentage of NA cells in raster is higher than 90%, -> NA
    NA
  } else {
    d = dim(sim1)
    sim1 = matrix(values(sim1), d[1], d[2], byrow=TRUE) %>%
      rast()
    sim1
  }
}

# my_sims_clc = data.frame(id = clc2018_2_lsp$id, my_sims = NA) #here you can add id = clc2018_2_lsp$id[2000:3000]
# my_sims_clc$my_sims = map(clc2018_2_lsp$id, lsp_extract2)
# 
# my_sims_clc = subset(my_sims_clc, my_sims != "NA") #removes empty cells
# my_sims2 = do.call(c, my_sims_clc$my_sims)

# plot(my_sims_clc$my_sims[[39]])

# test1 = lsp_extract(clc2018_2, window = 100, id = 2993)
# plot(test1)

# for (i in my_sims_clc$my_sims){
#   print(ext(i))
# }
# ext(my_sims_clc$my_sims[[39]])

# writeRaster(agg, "data/test22.tif", overwrite = TRUE)
# my_sims3 = raster::stack("data/test22.tif")


# calculate metrics -------------------------------------------------------
# my_sims_clc$ent = lsm_l_ent(my_sims2)$value
# my_sims_clc$mutinf = lsm_l_mutinf(my_sims2)$value
# my_sims_clc$relmutinf = lsm_l_relmutinf(my_sims2)$value
# 
# ggplot(my_sims_clc, aes(ent, relmutinf)) +
#   geom_point()









clc2018 = terra::rast("data/clc2018.tif")
show(clc2018)

clc2018 = project(clc2018, clc2018_2)

categories = unique(clc2018) %>% 
  unlist() %>% 
  unname()


# for (i in categories){
#   clc2018_3 = terra::classify(clc2018, cbind(i, i), others = (as.integer(i) + 1))
# }
i = categories[3]
clc2018_3 = terra::classify(clc2018, cbind(i, i), others = (as.integer(i) + 1))
freq(clc2018_3)
#1. divide into tiles
# 2. check which ones contain both categories
# 3. remove tiles that don't

lsp_extract3 = function(id){
  sim1 = lsp_extract(clc2018_3, window = window_size, id = id)
  
  na_test = sum(is.na(sim1[]))
  if (nrow(unique(sim1)) != 2){ #if two categories not present in tile -> NA
    NA
  } else if (any(dim(sim1)[1:2] != window_size)){ #if tile dimensions are not 100x100 -> NA
    NA
  } else if ((na_test / (ncell(sim1))) > 0.9){ #if the percentage of NA cells in raster is higher than 90%, -> NA
    NA
  } else {
    d = dim(sim1)
    sim1 = matrix(values(sim1), d[1], d[2], byrow=TRUE) %>% 
      rast()
    sim1
  }
}

my_sims_clc3 = data.frame(id = clc2018_2_lsp$id, my_sims = NA) #here you can add id = clc2018_2_lsp$id[2000:3000]
my_sims_clc3$my_sims = map(clc2018_2_lsp$id, lsp_extract3)

my_sims_clc3 = subset(my_sims_clc3, my_sims != "NA") #removes empty cells
my_sims2 = do.call(c, my_sims_clc3$my_sims)

my_sims_clc3$ent = lsm_l_ent(my_sims2)$value
my_sims_clc3$mutinf = lsm_l_mutinf(my_sims2)$value
my_sims_clc3$relmutinf = lsm_l_relmutinf(my_sims2)$value

ggplot(my_sims_clc3, aes(ent, relmutinf)) +
  geom_point()
