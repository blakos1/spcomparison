# devtools::install_github("cran/RandomFields")
# devtools::install_github("ropensci/NLMR")
# devtools::install_github("ropensci/landscapetools")
library(terra)
library(purrr)
library(NLMR)
library(tmap)
library(landscapetools)
library(landscapemetrics)
library(ggplot2)
library(motif)
library(tidyr)
library(comat)
library(philentropy)
# simulate ----------------------------------------------------------------
simulate_composition = function(fract_dim){
  sim1 = nlm_fbm(100, 100, fract_dim = fract_dim)
  sim1
}

param_df = expand.grid(fract_dim = seq(0.1, 1.8, by = 0.3))

set.seed(2022-06-13)
my_sims = map(param_df$fract_dim, simulate_composition)


param_df2 = expand.grid(breaks = seq(0.01, 0.5, length.out = 6),
                        fract_dim = seq(0.1, 1.8, by = 0.3))
param_df2$my_sims = rep(my_sims, each = 6)


simulate_configuration = function(x, breaks){
  sim1 = x |> 
    landscapetools::util_binarize(breaks) |> 
    rast()
  crs(sim1) = "EPSG:2180"
  sim1
}

param_df2$my_sims2 = map2(param_df2$my_sims, param_df2$breaks, simulate_configuration)


my_sims2 = do.call(c, param_df2$my_sims2)

# viz ---------------------------------------------------------------------
writeRaster(my_sims2, "my_sims2.tif")
my_sims3 = raster::stack("my_sims2.tif")
tm_shape(my_sims3) +
  tm_raster(legend.show = FALSE) +
  tm_facets(ncol = 6)

# calculate metrics -------------------------------------------------------
param_df2$ent = lsm_l_ent(my_sims3)$value
param_df2$mutinf = lsm_l_mutinf(my_sims3)$value
param_df2$relmutinf = lsm_l_relmutinf(my_sims3)$value

ggplot(param_df2, aes(ent, relmutinf)) +
  geom_point()

# distances ---------------------------------------------------------------
param_df2$id = seq_along(param_df2$breaks)
compare_dist = function(r1, r2, method = "jensen-shannon", unit = "log2"){
  r1m = as.matrix(r1, wide = TRUE)
  r2m = as.matrix(r2, wide = TRUE)
  
  r1mc = get_cove(get_coma(r1m), normalization = "pdf")
  r2mc = get_cove(get_coma(r2m), normalization = "pdf")
  
  dist_one_one(r1mc, r2mc, method = method, unit = unit)
}

dist_mat = matrix(nrow = nrow(param_df2), ncol = nrow(param_df2))
for (i in param_df2$id){
  for (j in param_df2$id){
    dist_mat[i, j] = compare_dist(my_sims2[[i]], my_sims2[[j]], method = "jensen-shannon")
  }
}

dist_df = as.data.frame(dist_mat)
dist_df$id = seq_along(dist_df$V1)
dist_df_long = pivot_longer(dist_df, 1:36)
dist_df_long$name = as.numeric(gsub("V", "", dist_df_long$name))
